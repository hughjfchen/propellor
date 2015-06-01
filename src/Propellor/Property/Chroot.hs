{-# LANGUAGE FlexibleContexts #-}

module Propellor.Property.Chroot (
	Chroot(..),
	BuilderConf(..),
	debootstrapped,
	provisioned,
	-- * Internal use
	provisioned',
	propigateChrootInfo,
	propellChroot,
	chain,
) where

import Propellor
import Propellor.Types.CmdLine
import Propellor.Types.Chroot
import Propellor.Property.Chroot.Util
import qualified Propellor.Property.Debootstrap as Debootstrap
import qualified Propellor.Property.Systemd.Core as Systemd
import qualified Propellor.Shim as Shim
import Propellor.Property.Mount

import qualified Data.Map as M
import Data.List.Utils
import System.Posix.Directory

data Chroot = Chroot FilePath System BuilderConf Host
	deriving (Show)

data BuilderConf
	= UsingDeboostrap Debootstrap.DebootstrapConfig
	deriving (Show)

instance PropAccum Chroot where
	(Chroot l s c h) & p = Chroot l s c (h & p)
	(Chroot l s c h) &^ p = Chroot l s c (h &^ p)
	getProperties (Chroot _ _ _ h) = hostProperties h

-- | Defines a Chroot at the given location, built with debootstrap.
--
-- Properties can be added to configure the Chroot.
--
-- > debootstrapped (System (Debian Unstable) "amd64") Debootstrap.BuildD "/srv/chroot/ghc-dev"
-- >    & Apt.installed ["ghc", "haskell-platform"]
-- >	& ...
debootstrapped :: System -> Debootstrap.DebootstrapConfig -> FilePath -> Chroot
debootstrapped system conf location = case system of
	(System (Debian _) _) -> mk
	(System (Ubuntu _) _) -> mk
  where
	h = Host location [] mempty
	mk = Chroot location system (UsingDeboostrap conf) h
		& os system

-- | Ensures that the chroot exists and is provisioned according to its
-- properties.
--
-- Reverting this property removes the chroot. Anything mounted inside it
-- is first unmounted. Note that it does not ensure that any processes
-- that might be running inside the chroot are stopped.
provisioned :: Chroot -> RevertableProperty
provisioned c = provisioned' (propigateChrootInfo c) c False

provisioned' :: (Property HasInfo -> Property HasInfo) -> Chroot -> Bool -> RevertableProperty
provisioned' propigator c@(Chroot loc system builderconf _) systemdonly =
	(propigator $ go "exists" setup)
		<!>
	(go "removed" teardown)
  where
	go desc a = propertyList (chrootDesc c desc) [a]

	setup = propellChroot c (inChrootProcess (not systemdonly) c) systemdonly
		`requires` toProp built
	
	built = case (system, builderconf) of
		((System (Debian _) _), UsingDeboostrap cf) -> debootstrap cf
		((System (Ubuntu _) _), UsingDeboostrap cf) -> debootstrap cf

	debootstrap = Debootstrap.built loc system

	teardown = toProp (revert built)

propigateChrootInfo :: (IsProp (Property i)) => Chroot -> Property i -> Property HasInfo
propigateChrootInfo c p = propigateContainer c p'
  where
	p' = infoProperty
		(propertyDesc p)
		(propertySatisfy p)
		(propertyInfo p <> chrootInfo c)
		(propertyChildren p)

chrootInfo :: Chroot -> Info
chrootInfo (Chroot loc _ _ h) =
	mempty { _chrootinfo = mempty { _chroots = M.singleton loc h } }

-- | Propellor is run inside the chroot to provision it.
propellChroot :: Chroot -> ([String] -> IO (CreateProcess, IO ())) -> Bool -> Property NoInfo
propellChroot c@(Chroot loc _ _ _) mkproc systemdonly = property (chrootDesc c "provisioned") $ do
	let d = localdir </> shimdir c
	let me = localdir </> "propellor"
	shim <- liftIO $ ifM (doesDirectoryExist d)
		( pure (Shim.file me d)
		, Shim.setup me Nothing d
		)
	ifM (liftIO $ bindmount shim)
		( chainprovision shim
		, return FailedChange
		)
  where
	bindmount shim = ifM (doesFileExist (loc ++ shim))
		( return True
		, do
			let mntpnt = loc ++ localdir
			createDirectoryIfMissing True mntpnt
			boolSystem "mount"
				[ Param "--bind"
				, File localdir, File mntpnt
				]
		)

	chainprovision shim = do
		parenthost <- asks hostName
		cmd <- liftIO $ toChain parenthost c systemdonly
		pe <- liftIO standardPathEnv
		(p, cleanup) <- liftIO $ mkproc
			[ shim
			, "--continue"
			, show cmd
			]
		let p' = p { env = Just pe }
		r <- liftIO $ withHandle StdoutHandle createProcessSuccess p'
			processChainOutput
		liftIO cleanup
		return r

toChain :: HostName -> Chroot -> Bool -> IO CmdLine
toChain parenthost (Chroot loc _ _ _) systemdonly = do
	onconsole <- isConsole <$> mkMessageHandle
	return $ ChrootChain parenthost loc systemdonly onconsole

chain :: [Host] -> CmdLine -> IO ()
chain hostlist (ChrootChain hn loc systemdonly onconsole) = 
	case findHostNoAlias hostlist hn of
		Nothing -> errorMessage ("cannot find host " ++ hn)
		Just parenthost -> case M.lookup loc (_chroots $ _chrootinfo $ hostInfo parenthost) of
			Nothing -> errorMessage ("cannot find chroot " ++ loc ++ " on host " ++ hn)
			Just h -> go h
  where
	go h = do
		changeWorkingDirectory localdir
		when onconsole forceConsole
		onlyProcess (provisioningLock loc) $ do
			r <- runPropellor h $ ensureProperties $
				if systemdonly
					then [Systemd.installed]
					else map ignoreInfo $
						hostProperties h
			putStrLn $ "\n" ++ show r
chain _ _ = errorMessage "bad chain command"

inChrootProcess :: Bool -> Chroot -> [String] -> IO (CreateProcess, IO ())
inChrootProcess keepprocmounted (Chroot loc _ _ _) cmd = do
	mountproc
	return (proc "chroot" (loc:cmd), cleanup)
  where
	-- /proc needs to be mounted in the chroot for the linker to use
	-- /proc/self/exe which is necessary for some commands to work
	mountproc = unlessM (elem procloc <$> mountPointsBelow loc) $
		void $ mount "proc" "proc" procloc
	
	procloc = loc </> "proc"

	cleanup
		| keepprocmounted = noop
		| otherwise = whenM (elem procloc <$> mountPointsBelow loc) $
			umountLazy procloc

provisioningLock :: FilePath -> FilePath
provisioningLock containerloc = "chroot" </> mungeloc containerloc ++ ".lock"

shimdir :: Chroot -> FilePath
shimdir (Chroot loc _ _ _) = "chroot" </> mungeloc loc ++ ".shim"

mungeloc :: FilePath -> String
mungeloc = replace "/" "_"

chrootDesc :: Chroot -> String -> String
chrootDesc (Chroot loc _ _ _) desc = "chroot " ++ loc ++ " " ++ desc

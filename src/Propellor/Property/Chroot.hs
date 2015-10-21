{-# LANGUAGE FlexibleContexts, GADTs #-}

module Propellor.Property.Chroot (
	Chroot(..),
	ChrootBootstrapper(..),
	Debootstrapped(..),
	debootstrapped,
	bootstrapped,
	provisioned,
	-- * Internal use
	provisioned',
	propagateChrootInfo,
	propellChroot,
	chain,
	chrootSystem,
) where

import Propellor.Base
import Propellor.Types.CmdLine
import Propellor.Types.Chroot
import Propellor.Types.Info
import Propellor.Property.Chroot.Util
import qualified Propellor.Property.Debootstrap as Debootstrap
import qualified Propellor.Property.Systemd.Core as Systemd
import qualified Propellor.Property.File as File
import qualified Propellor.Shim as Shim
import Propellor.Property.Mount

import qualified Data.Map as M
import Data.List.Utils
import System.Posix.Directory

-- | Specification of a chroot. Normally you'll use `debootstrapped` or
-- `bootstrapped` to construct a Chroot value.
data Chroot where
	Chroot :: ChrootBootstrapper b => FilePath -> System -> b -> Host -> Chroot

chrootSystem :: Chroot -> System
chrootSystem (Chroot _ system _ _) = system

instance Show Chroot where
	show (Chroot loc system _ _) = "Chroot " ++ loc ++ " " ++ show system

instance PropAccum Chroot where
	(Chroot l s c h) `addProp` p = Chroot l s c (h & p)
	(Chroot l s c h) `addPropFront` p = Chroot l s c (h `addPropFront` p)
	getProperties (Chroot _ _ _ h) = hostProperties h

-- | Class of things that can do initial bootstrapping of an operating
-- System in a chroot.
class ChrootBootstrapper b where
	-- | Do initial bootstrapping of an operating system in a chroot.
	-- If the operating System is not supported, return Nothing.
	buildchroot :: b -> System -> FilePath -> Maybe (Property HasInfo)

-- | Use to extract a tarball with a filesystem image.
--
-- The filesystem image is expected to be a root directory (no top-level
-- directory, also known as a "tarbomb"). It may be optionally compressed with
-- any format `tar` knows how to detect automatically.
data ChrootTarball = ChrootTarball FilePath

instance ChrootBootstrapper ChrootTarball where
	buildchroot (ChrootTarball tb) _ loc = Just $ extractTarball loc tb

extractTarball :: FilePath -> FilePath -> Property HasInfo
extractTarball target src = toProp .
	check (unpopulated target) $
		cmdProperty "tar" params
			`requires` File.dirExists target
  where
	params =
		[ "-C"
		, target
		, "-xf"
		, src
		]

-- | Use to bootstrap a chroot with debootstrap.
data Debootstrapped = Debootstrapped Debootstrap.DebootstrapConfig

instance ChrootBootstrapper Debootstrapped where
	buildchroot (Debootstrapped cf) system loc = case system of
		(System (Debian _) _) -> Just debootstrap
		(System (Ubuntu _) _) -> Just debootstrap
	  where
		debootstrap = Debootstrap.built loc system cf

-- | Defines a Chroot at the given location, built with debootstrap.
--
-- Properties can be added to configure the Chroot.
--
-- > debootstrapped (System (Debian Unstable) "amd64") Debootstrap.BuildD "/srv/chroot/ghc-dev"
-- > 	& Apt.installed ["ghc", "haskell-platform"]
-- > 	& ...
debootstrapped :: System -> Debootstrap.DebootstrapConfig -> FilePath -> Chroot
debootstrapped system conf = bootstrapped system (Debootstrapped conf)

-- | Defines a Chroot at the given location, bootstrapped with the
-- specified ChrootBootstrapper.
bootstrapped :: ChrootBootstrapper b => System -> b -> FilePath -> Chroot
bootstrapped system bootstrapper location =
	Chroot location system bootstrapper h
		& os system
  where
	h = Host location [] mempty

-- | Ensures that the chroot exists and is provisioned according to its
-- properties.
--
-- Reverting this property removes the chroot. Anything mounted inside it
-- is first unmounted. Note that it does not ensure that any processes
-- that might be running inside the chroot are stopped.
provisioned :: Chroot -> RevertableProperty
provisioned c = provisioned' (propagateChrootInfo c) c False

provisioned' :: (Property HasInfo -> Property HasInfo) -> Chroot -> Bool -> RevertableProperty
provisioned' propigator c@(Chroot loc system bootstrapper _) systemdonly =
	(propigator $ propertyList (chrootDesc c "exists") [setup])
		<!>
	(propertyList (chrootDesc c "removed") [teardown])
  where
	setup = propellChroot c (inChrootProcess (not systemdonly) c) systemdonly
		`requires` toProp built
	
	built = case buildchroot bootstrapper system loc of
		Just p -> p
		Nothing -> cantbuild

	cantbuild = infoProperty (chrootDesc c "built") (error $ "cannot bootstrap " ++ show system ++ " using supplied ChrootBootstrapper") mempty []

	teardown = check (not <$> unpopulated loc) $
		property ("removed " ++ loc) $
			makeChange (removeChroot loc)

propagateChrootInfo :: (IsProp (Property i)) => Chroot -> Property i -> Property HasInfo
propagateChrootInfo c@(Chroot location _ _ _) p = propagateContainer location c p'
  where
	p' = infoProperty
		(propertyDesc p)
		(propertySatisfy p)
		(propertyInfo p <> chrootInfo c)
		(propertyChildren p)

chrootInfo :: Chroot -> Info
chrootInfo (Chroot loc _ _ h) = mempty `addInfo` 
	mempty { _chroots = M.singleton loc h }

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
		Just parenthost -> case M.lookup loc (_chroots $ getInfo $ hostInfo parenthost) of
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

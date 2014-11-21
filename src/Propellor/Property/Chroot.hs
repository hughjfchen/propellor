module Propellor.Property.Chroot (
	Chroot(..),
	chroot,
	provisioned,
	-- * Internal use
	provisioned',
	propigateChrootInfo,
	propellChroot,
	chain,
) where

import Propellor
import qualified Propellor.Property.Debootstrap as Debootstrap
import qualified Propellor.Shim as Shim
import Utility.SafeCommand

import qualified Data.Map as M
import Data.List.Utils
import System.Posix.Directory

data Chroot = Chroot FilePath System Host
	deriving (Show)

instance Hostlike Chroot where
	(Chroot l s h) & p = Chroot l s (h & p)
	(Chroot l s h) &^ p = Chroot l s (h &^ p)
	getHost (Chroot _ _ h) = h

-- | Defines a Chroot at the given location, containing the specified
-- System. Properties can be added to configure the Chroot.
--
-- > chroot "/srv/chroot/ghc-dev" (System (Debian Unstable) "amd64")
-- >    & Apt.installed ["build-essential", "ghc", "haskell-platform"]
-- >	& ...
chroot :: FilePath -> System -> Chroot
chroot location system = Chroot location system (Host location [] mempty)
	& os system

-- | Ensures that the chroot exists and is provisioned according to its
-- properties.
--
-- Reverting this property removes the chroot. Note that it does not ensure
-- that any processes that might be running inside the chroot are stopped.
provisioned :: Chroot -> RevertableProperty
provisioned c = provisioned' (propigateChrootInfo c) c

provisioned' :: (Property -> Property) -> Chroot -> RevertableProperty
provisioned' propigator c@(Chroot loc system _) = RevertableProperty
	(propigator $ go "exists" setup)
	(go "removed" teardown)
  where
	go desc a = property (chrootDesc c desc) $ ensureProperties [a]

	setup = propellChroot c (inChrootProcess c) `requires` toProp built
	
	built = case system of
		(System (Debian _) _) -> debootstrap
		(System (Ubuntu _) _) -> debootstrap

	debootstrap = Debootstrap.built loc system []

	teardown = toProp (revert built)

propigateChrootInfo :: Chroot -> Property -> Property
propigateChrootInfo c p = propigateInfo c p (<> chrootInfo c)

chrootInfo :: Chroot -> Info
chrootInfo (Chroot loc _ h) =
	mempty { _chrootinfo = mempty { _chroots = M.singleton loc h } }

-- | Propellor is run inside the chroot to provision it.
propellChroot :: Chroot -> ([String] -> CreateProcess) -> Property
propellChroot c@(Chroot loc _ _) mkproc = property (chrootDesc c "provisioned") $ do
	let d = localdir </> shimdir c
	let me = localdir </> "propellor"
	shim <- liftIO $ ifM (doesDirectoryExist d)
		( pure (Shim.file me d)
		, Shim.setup me d
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
		cmd <- liftIO $ toChain parenthost c
		let p = mkproc
			[ shim
			, "--continue"
			, show cmd
			]
		liftIO $ withHandle StdoutHandle createProcessSuccess p
			processChainOutput

toChain :: HostName -> Chroot -> IO CmdLine
toChain parenthost (Chroot loc _ _) = do
	onconsole <- isConsole <$> mkMessageHandle
	return $ ChrootChain parenthost loc onconsole

chain :: [Host] -> HostName -> FilePath -> Bool -> IO ()
chain hostlist hn loc onconsole = case findHostNoAlias hostlist hn of
	Nothing -> errorMessage ("cannot find host " ++ hn)
	Just parenthost -> case M.lookup loc (_chroots $ _chrootinfo $ hostInfo parenthost) of
		Nothing -> errorMessage ("cannot find chroot " ++ loc ++ " on host " ++ hn)
		Just h -> go h
  where
	go h = do
		changeWorkingDirectory localdir
		when onconsole forceConsole
		onlyProcess (provisioningLock loc) $ do
			r <- runPropellor h $ ensureProperties $ hostProperties h
			putStrLn $ "\n" ++ show r

inChrootProcess :: Chroot -> [String] -> CreateProcess
inChrootProcess (Chroot loc _ _) cmd = proc "chroot" (loc:cmd)

provisioningLock :: FilePath -> FilePath
provisioningLock containerloc = "chroot" </> mungeloc containerloc ++ ".lock"

shimdir :: Chroot -> FilePath
shimdir (Chroot loc _ _) = "chroot" </> mungeloc loc ++ ".shim"

mungeloc :: FilePath -> String
mungeloc = replace "/" "_"

chrootDesc :: Chroot -> String -> String
chrootDesc (Chroot loc _ _) desc = "chroot " ++ loc ++ " " ++ desc

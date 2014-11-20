module Propellor.Property.Chroot (
	Chroot,
	chroot,
	provisioned,
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

instance Hostlike Chroot where
	(Chroot l s h) & p = Chroot l s (h & p)
	(Chroot l s h) &^ p = Chroot l s (h &^ p)
	getHost (Chroot _ _ h) = h

-- | Defines a Chroot at the given location, containing the specified
-- System. Properties can be added to configure the Chroot.
--
-- > chroot "/srv/chroot/ghc-dev" (System (Debian Unstable) "amd64"
-- >    & Apt.installed ["build-essential", "ghc", "haskell-platform"]
-- >	& ...
chroot :: FilePath -> System -> Chroot
chroot location system = Chroot location system (Host location [] mempty)

-- | Ensures that the chroot exists and is provisioned according to its
-- properties.
--
-- Reverting this property removes the chroot. Note that it does not ensure
-- that any processes that might be running inside the chroot are stopped.
provisioned :: Chroot -> RevertableProperty
provisioned c@(Chroot loc system _) = RevertableProperty
	(propigateChrootInfo c (go "exists" setup))
	(go "removed" teardown)
  where
	go desc a = property (chrootDesc c desc) $ ensureProperties [a]

	setup = provisionChroot c `requires` built
	
	built = case system of
		(System (Debian _) _) -> debootstrap
		(System (Ubuntu _) _) -> debootstrap

	debootstrap = unrevertable (Debootstrap.built loc system [])

	teardown = undefined

propigateChrootInfo :: Chroot -> Property -> Property
propigateChrootInfo c@(Chroot loc _ h) p = propigateInfo c p (<> chrootinfo)
  where
	chrootinfo = mempty $ mempty { _chroots = M.singleton loc h }

-- | Propellor is run inside the chroot to provision it.
--
-- Strange and wonderful tricks let the host's /usr/local/propellor
-- be used inside the chroot, without needing to install anything.
provisionChroot :: Chroot -> Property
provisionChroot c@(Chroot loc _ _) = property (chrootDesc c "provisioned") $ do
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
		let p = inChrootProcess c
			[ shim
			, "--continue"
			, show $ toChain parenthost c
			]
		liftIO $ withHandle StdoutHandle createProcessSuccess p
			processChainOutput

toChain :: HostName -> Chroot -> CmdLine
toChain parenthost (Chroot loc _ _) = ChrootChain parenthost loc

chain :: [Host] -> HostName -> FilePath -> IO ()
chain hostlist hn loc = case findHostNoAlias hostlist hn of
	Nothing -> errorMessage ("cannot find host " ++ hn)
	Just parenthost -> case M.lookup loc (_chroots $ _chrootinfo $ hostInfo parenthost) of
		Nothing -> errorMessage ("cannot find chroot " ++ loc ++ " on host " ++ hn)
		Just h -> go h
  where
	go h = do
		changeWorkingDirectory localdir
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

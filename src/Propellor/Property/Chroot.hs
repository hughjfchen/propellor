{-# LANGUAGE FlexibleContexts, GADTs, DeriveDataTypeable #-}

module Propellor.Property.Chroot (
	debootstrapped,
	bootstrapped,
	provisioned,
	hostChroot,
	Chroot(..),
	ChrootBootstrapper(..),
	Debootstrapped(..),
	ChrootTarball(..),
	noServices,
	inChroot,
	exposeTrueLocaldir,
	-- * Internal use
	provisioned',
	propagateChrootInfo,
	propellChroot,
	chain,
	chrootSystem,
) where

import Propellor.Base
import Propellor.Container
import Propellor.Types.CmdLine
import Propellor.Types.Chroot
import Propellor.Types.Info
import Propellor.Types.Core
import Propellor.Property.Chroot.Util
import qualified Propellor.Property.Debootstrap as Debootstrap
import qualified Propellor.Property.Systemd.Core as Systemd
import qualified Propellor.Property.File as File
import qualified Propellor.Shim as Shim
import Propellor.Property.Mount
import Utility.FileMode

import qualified Data.Map as M
import Data.List.Utils
import System.Posix.Directory
import System.Console.Concurrent

-- | Specification of a chroot. Normally you'll use `debootstrapped` or
-- `bootstrapped` to construct a Chroot value.
data Chroot where
	Chroot :: ChrootBootstrapper b => FilePath -> b -> InfoPropagator -> Host -> Chroot

instance IsContainer Chroot where
	containerProperties (Chroot _ _ _ h) = containerProperties h
	containerInfo (Chroot _ _ _ h) = containerInfo h
	setContainerProperties (Chroot loc b p h) ps =
		let h' = setContainerProperties h ps
		in Chroot loc b p h'

chrootSystem :: Chroot -> Maybe System
chrootSystem = fromInfoVal . fromInfo . containerInfo

instance Show Chroot where
	show c@(Chroot loc _ _ _) = "Chroot " ++ loc ++ " " ++ show (chrootSystem c)

-- | Class of things that can do initial bootstrapping of an operating
-- System in a chroot.
class ChrootBootstrapper b where
	-- | Do initial bootstrapping of an operating system in a chroot.
	-- If the operating System is not supported, return
	-- Left error message.
	buildchroot :: b -> Maybe System -> FilePath -> Either String (Property Linux)

-- | Use this to bootstrap a chroot by extracting a tarball.
--
-- The tarball is expected to contain a root directory (no top-level
-- directory, also known as a "tarbomb").
-- It may be optionally compressed with any format `tar` knows how to
-- detect automatically.
data ChrootTarball = ChrootTarball FilePath

instance ChrootBootstrapper ChrootTarball where
	buildchroot (ChrootTarball tb) _ loc = Right $
		tightenTargets $ extractTarball loc tb

extractTarball :: FilePath -> FilePath -> Property UnixLike
extractTarball target src = check (unpopulated target) $
	cmdProperty "tar" params
		`assume` MadeChange
		`requires` File.dirExists target
  where
	params =
		[ "-C"
		, target
		, "-xf"
		, src
		]

-- | Use this to bootstrap a chroot with debootstrap.
data Debootstrapped = Debootstrapped Debootstrap.DebootstrapConfig

instance ChrootBootstrapper Debootstrapped where
	buildchroot (Debootstrapped cf) system loc = case system of
		(Just s@(System (Debian _ _) _)) -> Right $ debootstrap s
		(Just s@(System (Buntish _) _)) -> Right $ debootstrap s
		(Just (System ArchLinux _)) -> Left "Arch Linux not supported by debootstrap."
		(Just (System (FreeBSD _) _)) -> Left "FreeBSD not supported by debootstrap."
		Nothing -> Left "Cannot debootstrap; OS not specified"
	  where
		debootstrap s = Debootstrap.built loc s cf

-- | Defines a Chroot at the given location, built with debootstrap.
--
-- Properties can be added to configure the Chroot. At a minimum,
-- add a property such as `osDebian` to specify the operating system
-- to bootstrap.
--
-- > debootstrapped Debootstrap.BuildD "/srv/chroot/ghc-dev" $ props
-- >	& osDebian Unstable X86_64
-- >	& Apt.installed ["ghc", "haskell-platform"]
-- >	& ...
debootstrapped :: Debootstrap.DebootstrapConfig -> FilePath -> Props metatypes -> Chroot
debootstrapped conf = bootstrapped (Debootstrapped conf)

-- | Defines a Chroot at the given location, bootstrapped with the
-- specified ChrootBootstrapper.
bootstrapped :: ChrootBootstrapper b => b -> FilePath -> Props metatypes -> Chroot
bootstrapped bootstrapper location ps = c
  where
	c = Chroot location bootstrapper propagateChrootInfo (host location ps)

-- | Ensures that the chroot exists and is provisioned according to its
-- properties.
--
-- Reverting this property removes the chroot. Anything mounted inside it
-- is first unmounted. Note that it does not ensure that any processes
-- that might be running inside the chroot are stopped.
provisioned :: Chroot -> RevertableProperty (HasInfo + Linux) Linux
provisioned c = provisioned' c False

provisioned'
	:: Chroot
	-> Bool
	-> RevertableProperty (HasInfo + Linux) Linux
provisioned' c@(Chroot loc bootstrapper infopropigator _) systemdonly =
	(infopropigator c normalContainerInfo $ setup `describe` chrootDesc c "exists")
		<!>
	(teardown `describe` chrootDesc c "removed")
  where
	setup :: Property Linux
	setup = propellChroot c (inChrootProcess (not systemdonly) c) systemdonly
		`requires` built

	built = case buildchroot bootstrapper (chrootSystem c) loc of
		Right p -> p
		Left e -> cantbuild e

	cantbuild e = property (chrootDesc c "built") (error e)

	teardown :: Property Linux
	teardown = check (not <$> unpopulated loc) $
		property ("removed " ++ loc) $
			makeChange (removeChroot loc)

type InfoPropagator = Chroot -> (PropagateInfo -> Bool) -> Property Linux -> Property (HasInfo + Linux)

propagateChrootInfo :: InfoPropagator
propagateChrootInfo c@(Chroot location _ _ _) pinfo p =
	propagateContainer location c pinfo $
		p `setInfoProperty` chrootInfo c

chrootInfo :: Chroot -> Info
chrootInfo (Chroot loc _ _ h) = mempty `addInfo`
	mempty { _chroots = M.singleton loc h }

-- | Propellor is run inside the chroot to provision it.
propellChroot :: Chroot -> ([String] -> IO (CreateProcess, IO ())) -> Bool -> Property UnixLike
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
	onconsole <- isConsole <$> getMessageHandle
	return $ ChrootChain parenthost loc systemdonly onconsole

chain :: [Host] -> CmdLine -> IO ()
chain hostlist (ChrootChain hn loc systemdonly onconsole) =
	case findHostNoAlias hostlist hn of
		Nothing -> errorMessage ("cannot find host " ++ hn)
		Just parenthost -> case M.lookup loc (_chroots $ fromInfo $ hostInfo parenthost) of
			Nothing -> errorMessage ("cannot find chroot " ++ loc ++ " on host " ++ hn)
			Just h -> go h
  where
	go h = do
		changeWorkingDirectory localdir
		when onconsole forceConsole
		onlyProcess (provisioningLock loc) $ do
			r <- runPropellor (setInChroot h) $ ensureChildProperties $
				if systemdonly
					then [toChildProperty Systemd.installed]
					else hostProperties h
			flushConcurrentOutput
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
		void $ mount "proc" "proc" procloc mempty

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

-- | Adding this property to a chroot prevents daemons and other services
-- from being started, which is often something you want to prevent when
-- building a chroot.
--
-- On Debian, this is accomplished by installing a </usr/sbin/policy-rc.d>
-- script that does not let any daemons be started by packages that use
-- invoke-rc.d. Reverting the property removes the script.
--
-- This property has no effect on non-Debian systems.
noServices :: RevertableProperty UnixLike UnixLike
noServices = setup <!> teardown
  where
	f = "/usr/sbin/policy-rc.d"
	script = [ "#!/bin/sh", "exit 101" ]
	setup = combineProperties "no services started" $ toProps
		[ File.hasContent f script
		, File.mode f (combineModes (readModes ++ executeModes))
		]
	teardown = File.notPresent f

-- | Check if propellor is currently running within a chroot.
--
-- This allows properties to check and avoid performing actions that
-- should not be done in a chroot.
inChroot :: Propellor Bool
inChroot = extract . fromMaybe (InChroot False) . fromInfoVal <$> askInfo
  where
	extract (InChroot b) = b

setInChroot :: Host -> Host
setInChroot h = h { hostInfo = hostInfo h `addInfo` InfoVal (InChroot True) }

newtype InChroot = InChroot Bool
	deriving (Typeable, Show)

-- | Runs an action with the true localdir exposed,
-- not the one bind-mounted into a chroot. The action is passed the
-- path containing the contents of the localdir outside the chroot.
--
-- In a chroot, this is accomplished by temporily bind mounting the localdir
-- to a temp directory, to preserve access to the original bind mount. Then
-- we unmount the localdir to expose the true localdir. Finally, to cleanup,
-- the temp directory is bind mounted back to the localdir.
exposeTrueLocaldir :: (FilePath -> Propellor a) -> Propellor a
exposeTrueLocaldir a = ifM inChroot
	( withTmpDirIn (takeDirectory localdir) "propellor.tmp" $ \tmpdir ->
		bracket_
			(movebindmount localdir tmpdir)
			(movebindmount tmpdir localdir)
			(a tmpdir)
	, a localdir
	)
  where
	movebindmount from to = liftIO $ do
		run "mount" [Param "--bind", File from, File to]
		-- Have to lazy unmount, because the propellor process
		-- is running in the localdir that it's unmounting..
		run "umount" [Param "-l", File from]
		-- We were in the old localdir; move to the new one after
		-- flipping the bind mounts. Otherwise, commands that try
		-- to access the cwd will fail because it got umounted out
		-- from under.
		changeWorkingDirectory "/"
		changeWorkingDirectory localdir
	run cmd ps = unlessM (boolSystem cmd ps) $
		error $ "exposeTrueLocaldir failed to run " ++ show (cmd, ps)

-- | Generates a Chroot that has all the properties of a Host.
-- 
-- Note that it's possible to create loops using this, where a host
-- contains a Chroot containing itself etc. Such loops will be detected at
-- runtime.
hostChroot :: ChrootBootstrapper bootstrapper => Host -> bootstrapper -> FilePath -> Chroot
hostChroot h bootstrapper d = chroot
  where
	chroot = Chroot d bootstrapper pinfo h
	pinfo = propagateHostChrootInfo h

-- This is different than propagateChrootInfo in that Info using
-- HostContext is not made to use the name of the chroot as its context,
-- but instead uses the hostname of the Host.
propagateHostChrootInfo :: Host -> InfoPropagator
propagateHostChrootInfo h c pinfo p =
	propagateContainer (hostName h) c pinfo $
		p `setInfoProperty` chrootInfo c

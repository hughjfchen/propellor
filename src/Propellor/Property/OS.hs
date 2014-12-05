module Propellor.Property.OS (
	cleanInstallOnce,
	Confirmation(..),
	preserveNetworkInterfaces,
	preserveRootSshAuthorized,
	grubBoots,
	GrubDev,
	rebootForced,
	kernelInstalled,
	oldOSRemoved,
) where

import Propellor
import qualified Propellor.Property.Debootstrap as Debootstrap
import qualified Propellor.Property.Ssh as Ssh
import qualified Propellor.Property.User as User
import Propellor.Property.Mount
import Propellor.Property.Chroot.Util (stdPATH)
import Utility.SafeCommand

import System.Posix.Files (rename, fileExist)
import Control.Exception (throw)

-- | Replaces whatever OS was installed before with a clean installation
-- of the OS that the Host is configured to have.
--
-- This can replace one Linux distribution with different one.
-- But, it can also fail and leave the system in an unbootable state.
--
-- To avoid this property being accidentially used, you have to provide
-- a Confirmation containing the name of the host that you intend to apply
-- the property to.
--
-- This property only runs once. The cleanly installed system will have
-- a file /etc/propellor-cleaninstall, which indicates it was cleanly
-- installed.
-- 
-- The files from the old os will be left in /old-os
--
-- TODO: A forced reboot should be schedued to run after propellor finishes
-- ensuring all properties of the host.
--
-- You will typically want to run some more properties after the clean
-- install succeeds, to bootstrap from the cleanly installed system to
-- a fully working system. For example:
--
-- > & os (System (Debian Unstable) "amd64")
-- > & cleanInstallOnce (Confirmed "foo.example.com")
-- >    `onChange` propertyList "fixing up after clean install"
-- >        [ preserveNetworkInterfaces
-- >        , preserverRootSshAuthorized
-- >        -- , kernelInstalled
-- >        -- , grubBoots "hd0"
-- >        -- , oldOsRemoved (Confirmed "foo.example.com")
-- >        ]
-- > & Apt.installed ["ssh"]
-- > & User.hasSomePassword "root"
-- > & User.accountFor "joey"
-- > & User.hasSomePassword "joey"
-- > -- rest of system properties here
cleanInstallOnce :: Confirmation -> Property
cleanInstallOnce confirmation = check (not <$> doesFileExist flagfile) $
	go `requires` confirmed "clean install confirmed" confirmation
  where
	go = 
		finalized
			`requires`
		propellorbootstrapped
			`requires`
		User.shadowConfig True
			`requires`
		flipped
			`requires`
		osbootstrapped

	osbootstrapped = withOS (newOSDir ++ " bootstrapped") $ \o -> case o of
		(Just d@(System (Debian _) _)) -> debootstrap d
		(Just u@(System (Ubuntu _) _)) -> debootstrap u
		_ -> error "os is not declared to be Debian or Ubuntu"
	debootstrap targetos = ensureProperty $ toProp $
		Debootstrap.built newOSDir targetos Debootstrap.DefaultConfig
	
	flipped = property (newOSDir ++ " moved into place") $ liftIO $ do
		-- First, unmount most mount points, lazily, so
		-- they don't interfere with moving things around.
		devfstype <- fromMaybe "devtmpfs" <$> getFsType "/dev"
		mnts <- filter (`notElem` ("/": trickydirs)) <$> mountPoints
		-- reverse so that deeper mount points come first
		forM_ (reverse mnts) umountLazy

		renamesout <- map (\d -> (d, oldOSDir ++ d, pure $ d `notElem` (oldOSDir:newOSDir:trickydirs)))
			<$> dirContents "/"
		renamesin <- map (\d -> let dest = "/" ++ takeFileName d in (d, dest, not <$> fileExist dest))
			<$> dirContents newOSDir
		createDirectoryIfMissing True oldOSDir
		massRename (renamesout ++ renamesin)
		removeDirectoryRecursive newOSDir
		
		-- Prepare environment for running additional properties.
		void $ setEnv "PATH" stdPATH True

		-- Remount /dev, so that block devices etc are
		-- available for other properties to use.
		unlessM (mount devfstype devfstype "/dev") $ do
			warningMessage $ "failed mounting /dev using " ++ devfstype ++ "; falling back to MAKEDEV generic"
			void $ boolSystem "sh" [Param "-c", Param "cd /dev && /sbin/MAKEDEV generic"]

		liftIO $ writeFile flagfile ""
		return MadeChange

	propellorbootstrapped = property "propellor re-debootstrapped in new os" $
		return NoChange
		-- re-bootstrap propellor in /usr/local/propellor,
		--   (using git repo bundle, privdata file, and possibly
		--   git repo url, which all need to be arranged to
		--   be present in /old-os's /usr/local/propellor)
		-- TODO
	
	-- Ensure that MadeChange is returned by the overall property,
	-- so that anything hooking in onChange will run afterwards.
	finalized = property "clean OS installed" $ return MadeChange

	flagfile = "/etc/propellor-cleaninstall"
	
	trickydirs = 
		-- /tmp can contain X's sockets, which prevent moving it
		-- so it's left as-is.
		[ "/tmp"
		-- /proc is left mounted
		, "/proc"
		]

-- Performs all the renames. If any rename fails, rolls back all
-- previous renames. Thus, this either successfully performs all
-- the renames, or does not change the system state at all.
massRename :: [(FilePath, FilePath, IO Bool)] -> IO ()
massRename = go []
  where
	go _ [] = return ()
	go undo ((from, to, test):rest) = ifM test
		( tryNonAsync (rename from to)
			>>= either
				(rollback undo)
				(const $ go ((to, from):undo) rest)
		, go undo rest
		)
	rollback undo e = do
		mapM_ (uncurry rename) undo
		throw e

data Confirmation = Confirmed HostName

confirmed :: Desc -> Confirmation -> Property
confirmed desc (Confirmed c) = property desc $ do
	hostname <- asks hostName
	if hostname /= c
		then do
			warningMessage "Run with a bad confirmation, not matching hostname."
			return FailedChange
		else return NoChange

-- | /etc/network/interfaces is configured to bring up all interfaces that
-- are currently up, using the same IP addresses.
preserveNetworkInterfaces :: Property
preserveNetworkInterfaces = undefined

-- | Root's .ssh/authorized_keys has added to it any ssh keys that
-- were authorized in the old OS. Any other contents of the file are
-- retained.
preserveRootSshAuthorized :: Property
preserveRootSshAuthorized = check (fileExist oldloc) $
	property (newloc ++ " copied from old OS") $ do
		ks <- liftIO $ lines <$> readFile oldloc
		ensureProperties (map (Ssh.authorizedKey "root") ks)
  where
	newloc = "/root/.ssh/authorized_keys"
	oldloc = oldOSDir ++ newloc

-- | Installs an appropriate kernel from the OS distribution.
kernelInstalled :: Property
kernelInstalled = undefined

-- | Installs grub onto a device to boot the system.
--
-- You may want to install grub to multiple devices; eg for a system
-- that uses software RAID.
grubBoots :: GrubDev -> Property
grubBoots = undefined

type GrubDev = String

-- | Forces an immediate reboot, without contacting the init system.
--
-- Can be used after cleanInstallOnce.
rebootForced :: Property
rebootForced = cmdProperty "reboot" [ "--force" ]

-- Removes the old OS's backup from /old-os
oldOSRemoved :: Confirmation -> Property
oldOSRemoved confirmation = check (doesDirectoryExist oldOSDir) $
	go `requires` confirmed "old OS backup removal confirmed" confirmation
  where
	go = property "old OS backup removed" $ do
		liftIO $ removeDirectoryRecursive oldOSDir
		return MadeChange

oldOSDir :: FilePath
oldOSDir = "/old-os"

newOSDir :: FilePath
newOSDir = "/new-os"

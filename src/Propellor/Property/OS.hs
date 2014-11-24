module Propellor.Property.OS (
	cleanInstallOnce,
	Confirmed(..),
	preserveNetworkInterfaces,
	preserveRootSshAuthorized,
	grubBoots,
	GrubDev(..),
	kernelInstalled,
	oldOSRemoved,
) where

import Propellor
import qualified Propellor.Property.Chroot as Chroot
import qualified Propellor.Property.Debootstrap as Debootstrap
import qualified Propellor.Property.File as File
import qualified Propellor.Property.Ssh as Ssh
import Utility.FileMode

-- | Replaces whatever OS was installed before with a clean installation
-- of the OS that the Host is configured to have.
--
-- This can replace one Linux distribution with different one.
-- But, it can also fail and leave the system in an unbootable state.
--
-- To avoid this property being accidentially used, you have to provide
-- a Confirmed containing the name of the host that you intend to apply the
-- property to.
--
-- This property only runs once. The cleanly installed system will have
-- a file /etc/propellor-cleaninstall, which indicates it was cleanly
-- installed.
--
-- You will typically want to run some more properties after the clean
-- install, to bootstrap from the cleanly installed system to a fully
-- working system. For example:
--
-- > & os (System (Debian Unstable) "amd64")
-- > & cleanInstall (Confirmed "foo.example.com") [BackupOldOS, UseOldKernel]
-- >    `onChange` propertyList "fixing up after clean install"
-- >        [ preserveNetworkInterfaces
-- >        , preserverRootSshAuthorized
-- >        -- , kernelInstalled
-- >        -- , grubBoots "hd0"
-- >        ]
-- > & Apt.installed ["ssh"]
-- > & User.hasSomePassword "root"
-- > & User.accountFor "joey"
-- > & User.hasSomePassword "joey"
-- > -- rest of system properties here
cleanInstallOnce :: Confirmed -> [Tweak] -> Property
cleanInstallOnce confirmed tweaks = check (not <$> doesFileExist flagfile) $
	property "OS cleanly installed" $ do
		checkConfirmed confirmed
		error "TODO"
		-- debootstrap /new-os chroot, but don't run propellor
		--   inside the chroot.
		-- unmount all mounts
		-- move all directories to /old-os,
		--   except for /boot and /lib/modules when UseOldKernel
		--   (or, delete when not BackupOldOS)
		-- move /new-os to /
		-- touch flagfile
		-- re-bootstrap propellor in /usr/local/propellor,
		--   (using git repo bundle, privdata file, and possibly
		--   git repo url, which all need to be arranged to
		--   be present in /old-os's /usr/local/propellor)
		-- enable shadow passwords (to avoid foot-shooting)
		-- return MadeChange
  where
	flagfile = "/etc/propellor-cleaninstall"

data Confirmed = Confirmed HostName

checkConfirmed :: Confirmed -> Propellor ()
checkConfirmed (Confirmed c) = do
	hostname <- asks hostName
	when (hostname /= c) $
		errorMessage "Run with a bad confirmation, not matching hostname."

-- | Sometimes you want an almost clean install, but with some tweaks.
data Tweak
	= UseOldKernel -- ^ Leave /boot and /lib/modules from old OS, so the system can boot using them as before
	| BackupOldOS -- ^ Back up old OS to /old-os, to avoid losing any important files

-- /etc/network/interfaces is configured to bring up all interfaces that
-- are currently up, using the same IP addresses.
preserveNetworkInterfaces :: Property
preserveNetworkInterfaces = undefined

-- Root's .ssh/authorized_keys has added to it any ssh keys that
-- were authorized in the old OS. Any other contents of the file are
-- retained.
preserveRootSshAuthorized :: Property
preserveRootSshAuthorized = check (doesDirectoryExist oldloc) $
	property (newloc ++ " copied from old OS") $ do
		ks <- liftIO $ lines <$> readFile oldloc
		ensureProperties (map (Ssh.authorizedKey "root") ks)
  where
	newloc = "/root/.ssh/authorized_keys"
	oldloc = oldOsDir ++ newloc

-- Installs an appropriate kernel from the OS distribution.
kernelInstalled :: Property
kernelInstalled = undefined

-- Installs grub onto a device to boot the system.
--
-- You may want to install grub to multiple devices; eg for a system
-- that uses software RAID.
grubBoots :: GrubDev -> Property
grubBoots = undefined

type GrubDev = String

-- Removes the old OS's backup from /old-os
oldOSRemoved :: Confirmed -> Property
oldOSRemoved confirmed = check (doesDirectoryExist oldOsDir) $
	property "old OS backup removed" $ do
		checkConfirmed confirmed
		liftIO $ removeDirectoryRecursive oldOsDir
		return MadeChange

oldOsDir :: FilePath
oldOsDir = "/old-os"

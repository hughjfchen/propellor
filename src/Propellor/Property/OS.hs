module Propellor.Property.OS (
	cleanInstallOnce,
	Confirmation
	confirm,
	fixupNetworkAddresses,
	fixupRootSsh,
	oldOSRemoved,
) where

import Propellor
import qualified Propellor.Property.Chroot as Chroot
import qualified Propellor.Property.Debootstrap as Debootstrap

-- | Replaces whatever OS was installed before with a clean installation
-- of the OS that the Host is configured to have.
--
-- This can replace one Linux distribution with different one.
-- But, it can also fail and leave the system in an unbootable state.
--
-- To avoid this property being accidentially used, you have to provide
-- a Context containing the name of the host that you intend to apply the
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
-- > & cleanInstall (Context "foo.example.com") (BackupOldOS <> UseOldKernel)
-- >    `onChange` propertyList "fixing up after clean install"
-- >        [ fixupNetworkInterfaces
-- >        , fixupRootSsh
-- >        -- , installDistroKernel
-- >        -- , installGrub
-- >        ]
-- > & Apt.installed ["ssh"]
-- > & User.hasSomePassword "root"
-- > & User.accountFor "joey"
-- > & User.hasSomePassword "joey"
-- > -- rest of system properties here
cleanInstallOnce :: Context -> Exceptions -> Property
cleanInstallOnce (Context c) = check (not <$> doesFileExist flagfile) $
	Property "OS cleanly installed" $ do
		hostname <- asks hostName
		when (hostname /= c) $
			error "Run with bad context, not matching hostname. Not running cleanInstalOnce!"
		error "TODO"
		-- debootstrap /new-os chroot, but don't run propellor
		--   inside the chroot.
		-- unmount all mounts
		-- move all directories to /old-os,
		--   except for /boot and /lib/modules
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

-- | Sometimes you want an almost clean install, but with some exceptions.
data Exceptions
	= UseOldKernel -- ^ Leave /boot and /lib/modules from old OS, so the system can boot using them as before
	| BackupOldOS -- ^ Back up old OS to /old-os, to avoid losing any important files
	| NoExceptions
	| CombinedExceptions Exceptions Exceptions

instance Monoid Exceptions where
	mempty = NoExceptions
	mappend = CombinedExceptions

-- /etc/network/interfaces is configured to bring up all interfaces that
-- are currently up, using the same IP addresses.
--
-- This property only does anything if it comes after cleanInstall,
-- in the same propellor run where cleanInstall has made a change.
fixupNetworkInterfaces :: Property
fixupNetworkInterfaces = undefined

-- /root/.ssh/authorized_keys is copied from the old os
fixupRootSsh :: Property
fixupRootSsh = undefined

-- Installs an appropriate kernel from the distribution.
installDistroKernel :: Property
installDistroKernel = undefined

-- Installs grub to boot the system.
installGrub :: Property
installGrub = undefined

-- Removes the old OS's backup from /old-os
oldOSRemoved :: Property
oldOSRemoved = check (doesDirectoryExist oldOsDir) $
	Property "old OS backup removed" $ liftIO $ do
		removeDirectoryRecursive oldOsDir
		return MadeChange

oldOsDir :: FilePath
oldOsDir = "/old-os"

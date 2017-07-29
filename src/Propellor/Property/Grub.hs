module Propellor.Property.Grub where

import Propellor.Base
import qualified Propellor.Property.File as File
import qualified Propellor.Property.Apt as Apt
import Propellor.Property.Mount
import Propellor.Property.Chroot (inChroot)
import Propellor.Types.Info
import Propellor.Types.Bootloader

-- | Eg, \"hd0,0\" or \"xen/xvda1\"
type GrubDevice = String

-- | Eg, \"\/dev/sda\"
type OSDevice = String

type TimeoutSecs = Int

-- | Types of machines that grub can boot.
data BIOS = PC | EFI64 | EFI32 | Coreboot | Xen

-- | Installs the grub package. This does not make grub be used as the
-- bootloader.
--
-- This includes running update-grub, unless it's run in a chroot.
installed :: BIOS -> Property (HasInfo + DebianLike)
installed bios = installed' bios 
	`onChange` (check (not <$> inChroot) mkConfig)

-- Run update-grub, to generate the grub boot menu. It will be
-- automatically updated when kernel packages are installed.
mkConfig :: Property DebianLike
mkConfig = tightenTargets $ cmdProperty "update-grub" []
	`assume` MadeChange

-- | Installs grub; does not run update-grub.
installed' :: BIOS -> Property (HasInfo + DebianLike)
installed' bios = setInfoProperty aptinstall
	(toInfo [GrubInstalled])
	`describe` "grub package installed"
  where
	aptinstall = Apt.installed [debpkg]
	debpkg = case bios of
		PC -> "grub-pc"
		EFI64 -> "grub-efi-amd64"
		EFI32 -> "grub-efi-ia32"
		Coreboot -> "grub-coreboot"
		Xen -> "grub-xen"

-- | Installs grub onto a device, so the system can boot from that device.
--
-- You may want to install grub to multiple devices; eg for a system
-- that uses software RAID.
--
-- Note that this property does not check if grub is already installed
-- on the device; it always does the work to reinstall it. It's a good idea
-- to arrange for this property to only run once, by eg making it be run
-- onChange after OS.cleanInstallOnce.
boots :: OSDevice -> Property Linux
boots dev = tightenTargets $ cmdProperty "grub-install" [dev]
	`assume` MadeChange
	`describe` ("grub boots " ++ dev)

-- | Use PV-grub chaining to boot
--
-- Useful when the VPS's pv-grub is too old to boot a modern kernel image.
--
-- <http://notes.pault.ag/linode-pv-grub-chainning/>
--
-- The rootdev should be in the form "hd0", while the bootdev is in the form
-- "xen/xvda".
chainPVGrub :: GrubDevice -> GrubDevice -> TimeoutSecs -> Property (HasInfo + DebianLike)
chainPVGrub rootdev bootdev timeout = combineProperties desc $ props
	& File.dirExists "/boot/grub"
	& "/boot/grub/menu.lst" `File.hasContent`
		[ "default 1" 
		, "timeout " ++ val timeout
		, ""
		, "title grub-xen shim"
		, "root (" ++ rootdev ++ ")"
		, "kernel /boot/xen-shim"
		, "boot"
		]
	& "/boot/load.cf" `File.hasContent`
		[ "configfile (" ++ bootdev ++ ")/boot/grub/grub.cfg" ]
	& installed Xen
	& flip flagFile "/boot/xen-shim" xenshim
  where
	desc = "chain PV-grub"
	xenshim = scriptProperty ["grub-mkimage --prefix '(" ++ bootdev ++ ")/boot/grub' -c /boot/load.cf -O x86_64-xen /usr/lib/grub/x86_64-xen/*.mod > /boot/xen-shim"]
		`assume` MadeChange
		`describe` "/boot-xen-shim"

-- | This is a version of `boots` that makes grub boot the system mounted
-- at a particular directory. The OSDevice should be the underlying disk
-- device that grub will be installed to (generally a whole disk, 
-- not a partition).
bootsMounted :: FilePath -> OSDevice -> Property Linux
bootsMounted mnt wholediskdev = combineProperties desc $ props
	-- remove mounts that are done below to make sure the right thing
	-- gets mounted
	& cleanupmounts
	-- bind mount host /dev so grub can access the loop devices
	& bindMount "/dev" (inmnt "/dev")
	& mounted "proc" "proc" (inmnt "/proc") mempty
	& mounted "sysfs" "sys" (inmnt "/sys") mempty
	-- update the initramfs so it gets the uuid of the root partition
	& inchroot "update-initramfs" ["-u"]
		`assume` MadeChange
	-- work around for http://bugs.debian.org/802717
	& check haveosprober (inchroot "chmod" ["-x", osprober])
	& inchroot "update-grub" []
		`assume` MadeChange
	& check haveosprober (inchroot "chmod" ["+x", osprober])
	& inchroot "grub-install" [wholediskdev]
		`assume` MadeChange
	& cleanupmounts
	-- sync all buffered changes out to the disk in case it's
	-- used right away
	& cmdProperty "sync" []
		`assume` NoChange
  where
	desc = "grub boots " ++ wholediskdev

  	-- cannot use </> since the filepath is absolute
	inmnt f = mnt ++ f

	inchroot cmd ps = cmdProperty "chroot" ([mnt, cmd] ++ ps)

	haveosprober = doesFileExist (inmnt osprober)
	osprober = "/etc/grub.d/30_os-prober"

	cleanupmounts :: Property Linux
	cleanupmounts = property desc $ liftIO $ do
		cleanup "/sys"
		cleanup "/proc"
		cleanup "/dev"
		return NoChange
	  where
		cleanup m = 
			let mp = inmnt m
			in whenM (isMounted mp) $
				umountLazy mp

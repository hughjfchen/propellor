{-# LANGUAGE FlexibleContexts #-}

module Propellor.Property.DiskImage (
	built,
	rebuilt,
	DiskImageConfig(..),
	DiskImageFinalization,
	grubBooted,
) where

import Propellor
import Propellor.Property.Chroot
import Propellor.Property.Parted

-- | Creates a bootable disk image.
--
-- First the specified Chroot is set up, and its properties are satisfied.
-- Then a disk image is created, large enough to fit the chroot, which
-- is copied into it. Finally, the DiskImageFinalization property is
-- satisfied to make the disk image bootable.
-- 
-- > let chroot d = Chroot.debootstrapped (System (Debian Unstable) "amd64") mempty d
-- > 	& Apt.installed ["openssh-server"]
-- >	& Grub.installed Grub.PC
-- >	& ...
-- > in DiskImage.built mempty chroot DiskImage.grubBooted
built :: DiskImageConfig -> (FilePath -> Chroot) -> DiskImageFinalization -> RevertableProperty
built = built' False

-- | Like 'built', but the chroot is deleted and rebuilt from scratch each
-- time. This is more expensive, but useful to ensure reproducible results
-- when the properties of the chroot have been changed.
rebuilt :: DiskImageConfig -> (FilePath -> Chroot) -> DiskImageFinalization -> RevertableProperty
rebuilt = built' True

built' :: Bool -> DiskImageConfig -> (FilePath -> Chroot) -> DiskImageFinalization -> RevertableProperty
built' rebuild c mkchroot final = undefined

data DiskImageConfig = DiskImageConfig
	{ freeSpace :: MegaBytes -- ^ A disk image is sized to fit the system installed in it. This adds some extra free space. (mempty default: 256 Megabytes)
	}

instance Monoid DiskImageConfig where
	mempty = DiskImageConfig (MegaBytes 256)
	mappend a b = a 
		{ freeSpace = freeSpace a <> freeSpace b
		}

-- | This is a property that is run, chrooted into the disk image. It's
-- typically only used to set up the boot loader.
type DiskImageFinalization = Property NoInfo

-- | Makes grub be the boot loader of the disk image.
--
-- This does not cause grub to be installed. Use `Grub.installed` when
-- setting up the Chroot to do that.
grubBooted :: DiskImageFinalization
grubBooted = undefined

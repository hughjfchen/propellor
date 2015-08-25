{-# LANGUAGE FlexibleContexts #-}

module Propellor.Property.DiskImage (
	built,
	DiskImageConfig(..),
	DiskImageFinalization,
	grubBooted,
) where

import Propellor
import Propellor.Property.Chroot
import Utility.DataUnits
import Data.Monoid

-- | Creates a bootable disk image.
--
-- First the specified Chroot is set up, then a disk image is created,
-- large enough to fit the chroot, which is copied into it. Finally, the
-- DiskImageFinalization property is satisfied to make the disk image
-- bootable.
-- 
-- > let chroot d = Chroot.debootstrapped (System (Debian Unstable) "amd64") Debootstrap.DefaultConfig d
-- > 	& Apt.installed ["openssh-server"]
-- >	& Grub.installed Grub.PC
-- >	& ...
-- > in DiskImage.built mempty chroot DiskImage.grubBooted
built :: DiskImageConfig -> (FilePath -> Chroot) -> DiskImageFinalization -> RevertableProperty
built c = undefined

data DiskImageConfig = DiskImageConfig
	{ freeSpace :: ByteSize -- ^ A disk image is sized to fit the system installed in it. This adds some extra free space.
	}

instance Monoid DiskImageConfig where
	-- | Default value is 256 mb freeSpace.
	mempty = DiskImageConfig (1024 * 1024 * 256)
	mappend a b = a 
		{ freeSpace = freeSpace a + freeSpace b
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

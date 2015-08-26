{-# LANGUAGE FlexibleContexts #-}

module Propellor.Property.DiskImage (
	built,
	rebuilt,
	MountPoint,
	MkPartTable,
	fitChrootSize,
	freeSpace,
	DiskImageFinalization,
	grubBooted,
	Grub.BIOS(..),
) where

import Propellor
import Propellor.Property.Chroot
import Propellor.Property.Parted
import qualified Propellor.Property.Grub as Grub

-- | Creates a bootable disk image.
--
-- First the specified Chroot is set up, and its properties are satisfied.
--
-- Then, the disk image is set up, and the chroot is copied into the
-- appropriate partition(s) of it.
--
-- Finally, the DiskImageFinalization property is
-- satisfied to make the disk image bootable.
-- 
-- > let chroot d = Chroot.debootstrapped (System (Debian Unstable) "amd64") mempty d
-- > 		& Apt.installed ["openssh-server"]
-- >		& ...
-- >    partitions = fitChrootSize MSDOS
-- >		[ (Just "/boot", mkPartiton EXT2)
-- >		, (Just "/", mkPartition EXT4)
-- >		, (Nothing, const (mkPartition LinuxSwap (MegaBytes 256)))
-- >		]
-- > in built chroot partitions (grubBooted PC)
built :: (FilePath -> Chroot) -> MkPartTable -> DiskImageFinalization -> RevertableProperty
built = built' False

-- | Like 'built', but the chroot is deleted and rebuilt from scratch each
-- time. This is more expensive, but useful to ensure reproducible results
-- when the properties of the chroot have been changed.
rebuilt :: (FilePath -> Chroot) -> MkPartTable -> DiskImageFinalization -> RevertableProperty
rebuilt = built' True

built' :: Bool -> (FilePath -> Chroot) -> MkPartTable -> DiskImageFinalization -> RevertableProperty
built' rebuild mkparttable mkchroot final = undefined

-- | Where a partition is mounted. Use Nothing for eg, LinuxSwap.
type MountPoint = Maybe FilePath

-- | This is provided with a list of the sizes of directories in the chroot
-- under each mount point. The input list corresponds to the list of mount
-- points that the function returns! This trick is accomplished by 
-- exploiting laziness and tying the knot.
--
-- (Partitions that are not mounted (ie, LinuxSwap) will have 128 MegaBytes
-- provides as a default size.)
type MkPartTable = [MegaBytes] -> ([MountPoint], PartTable)

-- TODO tie the knot
-- let f = fitChrootSize MSDOS [(Just "/", mkPartition  EXT2)] 
-- let (mnts, t) = f (map (MegaBytes . fromIntegral  . length . show) mnts)

-- | The constructor for each Partition is passed the size of the files
-- from the chroot that will be put in that partition.
-- 
-- Partitions that are not mounted (ie, LinuxSwap) will have their size
-- set to 128 MegaBytes, unless it's overridden.
fitChrootSize :: TableType -> [(MountPoint, MegaBytes -> Partition)] -> MkPartTable
fitChrootSize tt l basesizes = (mounts, parttable)
  where
	(mounts, sizers) = unzip l
	parttable = PartTable tt (map (uncurry id) (zip sizers basesizes))

-- | After populating the partitions with files from the chroot,
-- they will have remaining free space equal to the sizes of the input
-- partitions.
freeSpace :: TableType -> [(MountPoint, Partition)] -> MkPartTable
freeSpace tt = fitChrootSize tt . map (\(mnt, p) -> (mnt, adjustsz p))
  where
	adjustsz p basesize = p { partSize = partSize p <> basesize }

-- | A pair of properties. The first property is satisfied within the
-- chroot, and is typically used to download the boot loader.
-- The second property is satisfied chrooted into the resulting
-- disk image, and will typically take care of installing the boot loader
-- to the disk image.
type DiskImageFinalization = (Property NoInfo, Property NoInfo)

-- | Makes grub be the boot loader of the disk image.
grubBooted :: Grub.BIOS -> DiskImageFinalization
grubBooted bios = (Grub.installed bios, undefined)

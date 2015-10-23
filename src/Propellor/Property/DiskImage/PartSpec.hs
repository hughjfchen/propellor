-- | Disk image partition specification and combinators.

module Propellor.Property.DiskImage.PartSpec (
	module Propellor.Property.DiskImage.PartSpec,
	Partition,
	PartSize(..),
	PartFlag(..),
	TableType(..),
	Fs(..),
	MountPoint,
) where

import Propellor.Base
import Propellor.Property.Parted
import Propellor.Property.Mount

-- | Specifies a mount point and a constructor for a Partition.
-- 
-- The size that is eventually provided is the amount of space needed to 
-- hold the files that appear in the directory where the partition is to be
-- mounted. Plus a fudge factor, since filesystems have some space
-- overhead.
--
-- (Partitions that are not to be mounted (ie, LinuxSwap), or that have
-- no corresponding directory in the chroot will have 128 MegaBytes
-- provided as a default size.)
type PartSpec = (Maybe MountPoint, PartSize -> Partition)

defSz :: PartSize
defSz = MegaBytes 128

-- | Specifies a swap partition of a given size.
swapPartition :: PartSize -> PartSpec
swapPartition sz = (Nothing, const (mkPartition LinuxSwap sz))

-- | Specifies a partition with a given filesystem.
--
-- The partition is not mounted anywhere by default; use the combinators
-- below to configure it.
partition :: Fs -> PartSpec
partition fs = (Nothing, mkPartition fs)

-- | Specifies where to mount a partition.
mountedAt :: PartSpec -> FilePath -> PartSpec
mountedAt (_, p) mp = (Just mp, p)

-- | Adds additional free space to the partition.
addFreeSpace :: PartSpec -> PartSize -> PartSpec
addFreeSpace (mp, p) freesz = (mp, \sz -> p (sz <> freesz))

-- | Forced a partition to be a specific size, instead of scaling to the
-- size needed for the files in the chroot.
setSize :: PartSpec -> PartSize -> PartSpec
setSize (mp, p) sz = (mp, const (p sz))

-- | Sets a flag on the partition.
setFlag :: PartSpec -> PartFlag -> PartSpec
setFlag s f = adjustp s $ \p -> p { partFlags = (f, True):partFlags p }

-- | Makes a MSDOS partition be Extended, rather than Primary.
extended :: PartSpec -> PartSpec
extended s = adjustp s $ \p -> p { partType = Extended }

adjustp :: PartSpec -> (Partition -> Partition) -> PartSpec
adjustp (mp, p) f = (mp, f . p)

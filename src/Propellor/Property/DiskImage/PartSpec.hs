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

-- | Specifies a mount point, mount options, and a constructor for a Partition.
-- 
-- The size that is eventually provided is the amount of space needed to 
-- hold the files that appear in the directory where the partition is to be
-- mounted. Plus a fudge factor, since filesystems have some space
-- overhead.
type PartSpec = (Maybe MountPoint, MountOpts, PartSize -> Partition)

-- | Partitions that are not to be mounted (ie, LinuxSwap), or that have
-- no corresponding directory in the chroot will have 128 MegaBytes
-- provided as a default size.
defSz :: PartSize
defSz = MegaBytes 128

-- | Add 2% for filesystem overhead. Rationalle for picking 2%:
-- A filesystem with 1% overhead might just sneak by as acceptable.
-- Double that just in case. Add an additional 3 mb to deal with
-- non-scaling overhead of filesystems (eg, superblocks). 
-- Add an additional 200 mb for temp files, journals, etc.
fudge :: PartSize -> PartSize
fudge (MegaBytes n) = MegaBytes (n + n `div` 100 * 2 + 3 + 200)

-- | Specifies a swap partition of a given size.
swapPartition :: PartSize -> PartSpec
swapPartition sz = (Nothing, mempty, const (mkPartition LinuxSwap sz))

-- | Specifies a partition with a given filesystem.
--
-- The partition is not mounted anywhere by default; use the combinators
-- below to configure it.
partition :: Fs -> PartSpec
partition fs = (Nothing, mempty, mkPartition fs)

-- | Specifies where to mount a partition.
mountedAt :: PartSpec -> FilePath -> PartSpec
mountedAt (_, o, p) mp = (Just mp, o, p)

-- | Specifies a mount option, such as "noexec"
mountOpt :: ToMountOpts o => PartSpec -> o -> PartSpec
mountOpt (mp, o, p) o' = (mp, o <> toMountOpts o', p)

-- | Mount option to make a partition be remounted readonly when there's an
-- error accessing it.
errorReadonly :: MountOpts
errorReadonly = toMountOpts "errors=remount-ro"

-- | Adds additional free space to the partition.
addFreeSpace :: PartSpec -> PartSize -> PartSpec
addFreeSpace (mp, o, p) freesz = (mp, o, \sz -> p (sz <> freesz))

-- | Forced a partition to be a specific size, instead of scaling to the
-- size needed for the files in the chroot.
setSize :: PartSpec -> PartSize -> PartSpec
setSize (mp, o, p) sz = (mp, o, const (p sz))

-- | Sets a flag on the partition.
setFlag :: PartSpec -> PartFlag -> PartSpec
setFlag s f = adjustp s $ \p -> p { partFlags = (f, True):partFlags p }

-- | Makes a MSDOS partition be Extended, rather than Primary.
extended :: PartSpec -> PartSpec
extended s = adjustp s $ \p -> p { partType = Extended }

adjustp :: PartSpec -> (Partition -> Partition) -> PartSpec
adjustp (mp, o, p) f = (mp, o, f . p)

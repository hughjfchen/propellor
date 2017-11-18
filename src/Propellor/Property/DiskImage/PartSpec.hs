-- | Disk image partition specification and combinators.

module Propellor.Property.DiskImage.PartSpec (
	module Propellor.Types.PartSpec,
	module Propellor.Property.DiskImage.PartSpec,
	module Propellor.Property.Parted.Types,
	module Propellor.Property.Partition,
) where

import Propellor.Base
import Propellor.Property.Parted
import Propellor.Types.PartSpec
import Propellor.Property.Parted.Types
import Propellor.Property.Partition (Fs(..))
import Propellor.Property.Mount

-- | Specifies a partition with a given filesystem.
--
-- The partition is not mounted anywhere by default; use the combinators
-- below to configure it.
partition :: Monoid t => Fs -> PartSpec t
partition fs = (Nothing, mempty, mkPartition fs, mempty)

-- | Specifies a swap partition of a given size.
swapPartition :: Monoid t => PartSize -> PartSpec t
swapPartition sz = (Nothing, mempty, const (mkPartition LinuxSwap sz), mempty)

-- | Specifies where to mount a partition.
mountedAt :: PartSpec t -> FilePath -> PartSpec t
mountedAt (_, o, p, t) mp = (Just mp, o, p, t)

-- | Partitions in disk images default to being sized large enough to hold
-- the files that live in that partition.
--
-- This adds additional free space to a partition.
addFreeSpace :: PartSpec t -> PartSize -> PartSpec t
addFreeSpace (mp, o, p, t) freesz = (mp, o, p', t)
  where
	p' = \sz -> p (sz <> freesz)

-- | Specify a fixed size for a partition.
setSize :: PartSpec t -> PartSize -> PartSpec t
setSize (mp, o, p, t) sz = (mp, o, const (p sz), t)

-- | Specifies a mount option, such as "noexec"
mountOpt :: ToMountOpts o => PartSpec t -> o -> PartSpec t
mountOpt (mp, o, p, t) o' = (mp, o <> toMountOpts o', p, t)

-- | Mount option to make a partition be remounted readonly when there's an
-- error accessing it.
errorReadonly :: MountOpts
errorReadonly = toMountOpts "errors=remount-ro"

-- | Sets the percent of the filesystem blocks reserved for the super-user.
--
-- The default is 5% for ext2 and ext4. Some filesystems may not support
-- this.
reservedSpacePercentage :: PartSpec t -> Int -> PartSpec t
reservedSpacePercentage s percent = adjustp s $ \p -> 
	p { partMkFsOpts = ("-m"):show percent:partMkFsOpts p }

-- | Sets a flag on the partition.
setFlag :: PartSpec t -> PartFlag -> PartSpec t
setFlag s f = adjustp s $ \p -> p { partFlags = (f, True):partFlags p }

-- | Makes a MSDOS partition be Extended, rather than Primary.
extended :: PartSpec t -> PartSpec t
extended s = adjustp s $ \p -> p { partType = Extended }

adjustp :: PartSpec t -> (Partition -> Partition) -> PartSpec t
adjustp (mp, o, p, t) f = (mp, o, f . p, t)

adjustt :: PartSpec t -> (t -> t) -> PartSpec t
adjustt (mp, o, p, t) f = (mp, o, p, f t)

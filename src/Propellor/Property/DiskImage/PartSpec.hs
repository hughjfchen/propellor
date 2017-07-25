-- | Disk image partition specification and combinators.

-- Partitions in disk images default to being sized large enough to hold
-- the files that appear in the directory where the partition is to be
-- mounted. Plus a fudge factor, since filesystems have some space
-- overhead.

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

-- | Adds additional free space to the partition.
addFreeSpace :: PartSpec t -> PartSize -> PartSpec t
addFreeSpace (mp, o, p, t) freesz = (mp, o, p', t)
  where
	p' = \sz -> p (sz <> freesz)

-- | Add 2% for filesystem overhead. Rationalle for picking 2%:
-- A filesystem with 1% overhead might just sneak by as acceptable.
-- Double that just in case. Add an additional 3 mb to deal with
-- non-scaling overhead of filesystems (eg, superblocks). 
-- Add an additional 200 mb for temp files, journals, etc.
fudge :: PartSize -> PartSize
fudge (MegaBytes n) = MegaBytes (n + n `div` 100 * 2 + 3 + 200)

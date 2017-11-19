module Propellor.Types.PartSpec where

import Propellor.Property.Parted.Types
import Propellor.Property.Mount

-- | Specifies a mount point, mount options, and a constructor for a
-- Partition that determines its size.
type PartSpec t = (Maybe MountPoint, MountOpts, PartSize -> Partition, t)

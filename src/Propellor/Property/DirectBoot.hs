module Propellor.Property.DirectBoot(installed) where

import Propellor.Base
import Propellor.Types.Bootloader

installed :: Property (HasInfo + UnixLike)
installed = pureInfoProperty "direct boot" [DirectBoot]

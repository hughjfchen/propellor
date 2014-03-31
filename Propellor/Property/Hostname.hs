module Propellor.Property.Hostname where

import Propellor.Common
import qualified Propellor.Property.File as File

set :: HostName -> Property
set hostname = "/etc/hostname" `File.hasContent` [hostname]
	`onChange` cmdProperty "hostname" [Param hostname]
	`describe` ("hostname " ++ hostname)

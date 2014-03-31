module Propellor.Property.Hostname where

import Propellor
import qualified Propellor.Property.File as File

set :: HostName -> Property
set hostname = "/etc/hostname" `File.hasContent` [hostname]
	`onChange` cmdProperty "hostname" [hostname]
	`describe` ("hostname " ++ hostname)

module Property.Hostname where

import Property
import Utility.SafeCommand

type HostName = String

set :: HostName -> Property
set hostname = combineProperties ("hostname " ++ hostname)
	[ fileHasContent "/etc/hostname" [hostname]
	, cmdProperty "hostname" [Param hostname]
	]

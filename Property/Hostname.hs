module Property.Hostname where

import Property
import Utility.SafeCommand

type HostName = String

set :: HostName -> Property
set hostname = 
	fileHasContent "/etc/hostname" [hostname]
		`onChange` cmdProperty "hostname" [Param hostname]

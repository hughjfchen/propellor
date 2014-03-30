module Property.Hostname where

import Property
import qualified Property.File as File
import Utility.SafeCommand

type HostName = String

set :: HostName -> Property
set hostname = "/etc/hostname" `File.hasContent` [hostname]
	`onChange` cmdProperty "hostname" [Param hostname]

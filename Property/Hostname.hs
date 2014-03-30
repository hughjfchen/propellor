module Property.Hostname where

import Common
import qualified Property.File as File

type HostName = String

set :: HostName -> Property
set hostname = "/etc/hostname" `File.hasContent` [hostname]
	`onChange` cmdProperty "hostname" [Param hostname]

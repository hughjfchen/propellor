module Property.Hostname where

import Property

type HostName = String

set :: HostName -> Property
set hostname = fileHasContent "/etc/hostname" [hostname]

module Propellor.Property.Hostname where

import Propellor
import qualified Propellor.Property.File as File

-- | Sets the hostname. Should be provided with a FQDN, and will configure
-- both /etc/hostname (with the base hostname) and /etc/hosts (with the
-- full hostname). Also sets the current hostname.
set :: HostName -> Property
set hostname = "/etc/hostname" `File.hasContent` [hostname]
	`onChange` cmdProperty "hostname" [hostname]
	`describe` ("hostname " ++ hostname)
  where
	(host, domain) = separate (== '.') hostname

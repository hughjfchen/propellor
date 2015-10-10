module Propellor.Property.Systemd.Core where

import Propellor.Base
import qualified Propellor.Property.Apt as Apt

-- dbus is only a Recommends of systemd, but is needed for communication
-- from the systemd inside a container to the one outside, so make sure it
-- gets installed.
installed :: Property NoInfo
installed = Apt.installed ["systemd", "dbus"]

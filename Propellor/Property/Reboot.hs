module Propellor.Property.Reboot where

import Propellor.Common

now :: Property
now = cmdProperty "reboot" []
	`describe` "reboot now"

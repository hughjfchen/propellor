module Propellor.Property.Reboot where

import Propellor

now :: Property
now = cmdProperty "reboot" []
	`describe` "reboot now"

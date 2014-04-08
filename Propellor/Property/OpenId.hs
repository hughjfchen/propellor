module Propellor.Property.OpenId where

import Propellor
import qualified Propellor.Property.File as File
import qualified Propellor.Property.Apt as Apt

providerFor :: [UserName] -> Property
providerFor users = propertyList ("openid provider") $
	[ serviceRunning "apache2"
		`requires` Apt.installed ["apache2"]
	, Apt.installed ["simpleid"]
		`onChange` serviceRestarted "apache2"
	, serviceRestarted "apache2"
	] ++ map identfile users
  where
	identfile u = File.hasPrivContent $ concat
		[ "/var/lib/simpleid/identities/", u, ".identity" ]

module Propellor.Property.Postfix where

import Propellor
import qualified Propellor.Property.Apt as Apt
import Propellor.Property.User
import Utility.SafeCommand
import Utility.FileMode

import System.PosixCompat

installed :: Property
installed = Apt.serviceInstalledRunning "postfix"

-- | Configures postfix as a satellite system, which 
-- relats all mail through a relay host, which defaults to smtp.domain. 
--
-- The smarthost may refuse to relay mail on to other domains, without
-- futher coniguration/keys. But this should be enough to get cron job
-- mail flowing to a place where it will be seen.
satellite :: Property
satellite = Apt.reConfigure "postfix"
	[ ("postfix/main_mailer_type", "select", "Satellite system")
	, ("postfix/destinations", "string", "")
	]
	`describe` "postfix satellite system"
	`requires` installed

module Propellor.Property.Postfix where

import Propellor
import qualified Propellor.Property.Apt as Apt

installed :: Property
installed = Apt.serviceInstalledRunning "postfix"

-- | Configures postfix as a satellite system, which 
-- relats all mail through a relay host, which defaults to smtp.domain. 
--
-- The smarthost may refuse to relay mail on to other domains, without
-- futher coniguration/keys. But this should be enough to get cron job
-- mail flowing to a place where it will be seen.
satellite :: Property
satellite = setup `requires` installed
  where
	setup = trivial $ property "postfix satellite system" $ do
		hn <- getHostName
		ensureProperty $ Apt.reConfigure "postfix"
			[ ("postfix/main_mailer_type", "select", "Satellite system")
			, ("postfix/root_address", "string", "root")
			, ("postfix/destinations", "string", " ")
			, ("postfix/mailname", "string", hn)
			]

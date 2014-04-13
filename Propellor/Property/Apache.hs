module Propellor.Property.Apache where

import Propellor
import qualified Propellor.Property.File as File
import qualified Propellor.Property.Apt as Apt

type ConfigFile = [String]

siteEnabled :: HostName -> ConfigFile -> RevertableProperty
siteEnabled hn cf = RevertableProperty enable disable
  where
	enable = siteAvailable hn cf
		`onChange` cmdProperty "a2ensite" ["--quiet", hn]
		`requires` Apt.installed ["apache2"]
	disable = File.notPresent (siteCfg hn)
		`onChange` cmdProperty "a2dissite" ["--quiet", hn]

siteAvailable :: HostName -> ConfigFile -> Property
siteAvailable hn cf = siteCfg hn `File.hasContent` (comment:cf)
	`describe` ("apache site available " ++ hn)
  where
	comment = "# deployed with propellor, do not modify"

siteCfg :: HostName -> FilePath
siteCfg hn = "/etc/apache2/sites-available/" ++ hn ++ ".conf"

restart :: Property
restart = cmdProperty "service" ["apache2", "restart"]

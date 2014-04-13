module Propellor.Property.Apache where

import Propellor
import qualified Propellor.Property.File as File
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.Service as Service

type ConfigFile = [String]

siteEnabled :: HostName -> ConfigFile -> RevertableProperty
siteEnabled hn cf = RevertableProperty enable disable
  where
	enable = cmdProperty "a2ensite" ["--quiet", hn]
		`requires` siteAvailable hn cf
		`requires` installed
		`onChange` reloaded
	disable = File.notPresent (siteCfg hn)
		`onChange` cmdProperty "a2dissite" ["--quiet", hn]
		`requires` installed
		`onChange` reloaded

siteAvailable :: HostName -> ConfigFile -> Property
siteAvailable hn cf = siteCfg hn `File.hasContent` (comment:cf)
	`describe` ("apache site available " ++ hn)
  where
	comment = "# deployed with propellor, do not modify"

modEnabled :: String -> RevertableProperty
modEnabled modname = RevertableProperty enable disable
  where
	enable = cmdProperty "a2enmod" ["--quiet", modname]
		`requires` installed
		`onChange` reloaded
	disable = cmdProperty "a2dismod" ["--quiet", modname]
		`requires` installed
		`onChange` reloaded

siteCfg :: HostName -> FilePath
siteCfg hn = "/etc/apache2/sites-available/" ++ hn

installed :: Property
installed = Apt.installed ["apache2"]

restarted :: Property
restarted = cmdProperty "service" ["apache2", "restart"]

reloaded :: Property
reloaded = Service.reloaded "apache2"

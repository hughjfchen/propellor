module Propellor.Property.Apache where

import Propellor
import qualified Propellor.Property.File as File
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.Service as Service

type ConfigFile = [String]

siteEnabled :: HostName -> ConfigFile -> RevertableProperty
siteEnabled hn cf = RevertableProperty enable disable
  where
	enable = trivial $ cmdProperty "a2ensite" ["--quiet", hn]
		`describe` ("apache site enabled " ++ hn)
		`requires` siteAvailable hn cf
		`requires` installed
		`onChange` reloaded
	disable = trivial $ File.notPresent (siteCfg hn)
		`describe` ("apache site disabled " ++ hn)
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
	enable = trivial $ cmdProperty "a2enmod" ["--quiet", modname]
		`describe` ("apache module enabled " ++ modname)
		`requires` installed
		`onChange` reloaded
	disable = trivial $ cmdProperty "a2dismod" ["--quiet", modname]
		`describe` ("apache module disabled " ++ modname)
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

-- | Configure apache to use SNI to differentiate between
-- https hosts.
multiSSL :: Property
multiSSL = "/etc/apache2/conf.d/ssl" `File.hasContent`
	[ "NameVirtualHost *:443"
	, "SSLStrictSNIVHostCheck off"
	]
	`describe` "apache SNI enabled"
	`onChange` reloaded

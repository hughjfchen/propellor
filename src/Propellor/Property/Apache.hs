module Propellor.Property.Apache where

import Propellor
import qualified Propellor.Property.File as File
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.Service as Service

type ConfigFile = [String]

siteEnabled :: HostName -> ConfigFile -> RevertableProperty
siteEnabled hn cf = enable <!> disable
  where
	enable = combineProperties ("apache site enabled " ++ hn)
		[ siteAvailable hn cf
			`requires` installed
			`onChange` reloaded
		, check (not <$> isenabled) $
			cmdProperty "a2ensite" ["--quiet", hn]
				`requires` installed
				`onChange` reloaded
		]
	disable = combineProperties
		("apache site disabled " ++ hn) 
		(map File.notPresent (siteCfg hn))
		`onChange` cmdProperty "a2dissite" ["--quiet", hn]
		`requires` installed
		`onChange` reloaded
	isenabled = boolSystem "a2query" [Param "-q", Param "-s", Param hn]

siteAvailable :: HostName -> ConfigFile -> Property NoInfo
siteAvailable hn cf = combineProperties ("apache site available " ++ hn) $
	map (`File.hasContent` (comment:cf)) (siteCfg hn)
  where
	comment = "# deployed with propellor, do not modify"

modEnabled :: String -> RevertableProperty
modEnabled modname = enable <!> disable
  where
	enable = check (not <$> isenabled) $
		cmdProperty "a2enmod" ["--quiet", modname]
			`describe` ("apache module enabled " ++ modname)
			`requires` installed
			`onChange` reloaded
	disable = check isenabled $ 
		cmdProperty "a2dismod" ["--quiet", modname]
			`describe` ("apache module disabled " ++ modname)
			`requires` installed
			`onChange` reloaded
	isenabled = boolSystem "a2query" [Param "-q", Param "-m", Param modname]

-- This is a list of config files because different versions of apache
-- use different filenames. Propellor simply writes them all.
siteCfg :: HostName -> [FilePath]
siteCfg hn =
	-- Debian pre-2.4
	[ "/etc/apache2/sites-available/" ++ hn
	-- Debian 2.4+
	, "/etc/apache2/sites-available/" ++ hn ++ ".conf"
	] 

installed :: Property NoInfo
installed = Apt.installed ["apache2"]

restarted :: Property NoInfo
restarted = Service.restarted "apache2"

reloaded :: Property NoInfo
reloaded = Service.reloaded "apache2"

-- | Configure apache to use SNI to differentiate between
-- https hosts.
--
-- This was off by default in apache 2.2.22. Newver versions enable
-- it by default. This property uses the filename used by the old version.
multiSSL :: Property NoInfo
multiSSL = check (doesDirectoryExist "/etc/apache2/conf.d") $
	"/etc/apache2/conf.d/ssl" `File.hasContent`
		[ "NameVirtualHost *:443"
		, "SSLStrictSNIVHostCheck off"
		]
		`describe` "apache SNI enabled"
		`onChange` reloaded

-- | Config file fragment that can be inserted into a <Directory>
-- stanza to allow global read access to the directory.
--
-- Works with multiple versions of apache that have different ways to do
-- it.
allowAll :: String
allowAll = unlines
	[ "<IfVersion < 2.4>"
	, "Order allow,deny"
	, "allow from all"
	, "</IfVersion>"
	, "<IfVersion >= 2.4>"
	, "Require all granted"
	, "</IfVersion>"
	]

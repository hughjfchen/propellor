module Propellor.Property.Nginx where

import Propellor
import qualified Propellor.Property.File as File
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.Service as Service

type ConfigFile = [String]

siteEnabled :: HostName -> ConfigFile -> RevertableProperty
siteEnabled hn cf = RevertableProperty enable disable
  where
	enable = trivial (cmdProperty "ln" ["-s", siteValRelativeCfg hn, siteVal hn])
		`describe` ("nginx site enabled " ++ hn)
		`requires` siteAvailable hn cf
		`requires` installed
		`onChange` reloaded
	disable = trivial $ 
		("nginx site disabled " ++ hn) ==>
		    File.notPresent (siteCfg hn)
		`onChange` cmdProperty "rm" [siteVal hn]
		`requires` installed
		`onChange` reloaded

siteAvailable :: HostName -> ConfigFile -> Property
siteAvailable hn cf = ("nginx site available " ++ hn) ==>
	siteCfg hn `File.hasContent` (comment : cf)
  where
	comment = "# deployed with propellor, do not modify"

siteCfg :: HostName -> FilePath
siteCfg hn = "/etc/nginx/sites-available/" ++ hn

siteVal :: HostName -> FilePath
siteVal hn = "/etc/nginx/sites-enabled/" ++ hn

siteValRelativeCfg :: HostName -> FilePath
siteValRelativeCfg hn = "../sites-available/" ++ hn

installed :: Property
installed = Apt.installed ["nginx"]

restarted :: Property
restarted = Service.restarted "nginx"

reloaded :: Property
reloaded = Service.reloaded "nginx"

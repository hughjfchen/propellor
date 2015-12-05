-- | Maintainer: Félix Sipma <felix+propellor@gueux.org>

module Propellor.Property.Nginx where

import Propellor.Base
import qualified Propellor.Property.File as File
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.Service as Service

type ConfigFile = [String]

siteEnabled :: HostName -> ConfigFile -> RevertableProperty NoInfo
siteEnabled hn cf = enable <!> disable
  where
	enable = siteVal hn `File.isSymlinkedTo` siteValRelativeCfg hn
		`describe` ("nginx site enabled " ++ hn)
		`requires` siteAvailable hn cf
		`requires` installed
		`onChange` reloaded
	disable = File.notPresent (siteVal hn)
		`describe` ("nginx site disable" ++ hn)
		`requires` installed
		`onChange` reloaded

siteAvailable :: HostName -> ConfigFile -> Property NoInfo
siteAvailable hn cf = ("nginx site available " ++ hn) ==>
	siteCfg hn `File.hasContent` (comment : cf)
  where
	comment = "# deployed with propellor, do not modify"

siteCfg :: HostName -> FilePath
siteCfg hn = "/etc/nginx/sites-available/" ++ hn

siteVal :: HostName -> FilePath
siteVal hn = "/etc/nginx/sites-enabled/" ++ hn

siteValRelativeCfg :: HostName -> File.LinkTarget
siteValRelativeCfg hn = File.LinkTarget ("../sites-available/" ++ hn)

installed :: Property NoInfo
installed = Apt.installed ["nginx"]

restarted :: Property NoInfo
restarted = Service.restarted "nginx"

reloaded :: Property NoInfo
reloaded = Service.reloaded "nginx"

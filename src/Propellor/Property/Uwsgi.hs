-- | Maintainer: Félix Sipma <felix+propellor@gueux.org>

module Propellor.Property.Uwsgi where

import Propellor
import qualified Propellor.Property.File as File
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.Service as Service
import System.Posix.Files

type ConfigFile = [String]

appEnabled :: HostName -> ConfigFile -> RevertableProperty
appEnabled hn cf = enable <!> disable
  where
	enable = check test prop
		`describe` ("uwsgi app enabled " ++ hn)
		`requires` appAvailable hn cf
		`requires` installed
		`onChange` reloaded
	  where
		test = not <$> doesFileExist (appVal hn)
		prop = property "uwsgi app in place" $ makeChange $
			createSymbolicLink target dir
		target = appValRelativeCfg hn
		dir = appVal hn
	disable = trivial $ File.notPresent (appVal hn)
		`describe` ("uwsgi app disable" ++ hn)
		`requires` installed
		`onChange` reloaded

appAvailable :: HostName -> ConfigFile -> Property NoInfo
appAvailable hn cf = ("uwsgi app available " ++ hn) ==>
	appCfg hn `File.hasContent` (comment : cf)
  where
	comment = "# deployed with propellor, do not modify"

appCfg :: HostName -> FilePath
appCfg hn = "/etc/uwsgi/apps-available/" ++ hn

appVal :: HostName -> FilePath
appVal hn = "/etc/uwsgi/apps-enabled/" ++ hn

appValRelativeCfg :: HostName -> FilePath
appValRelativeCfg hn = "../apps-available/" ++ hn

installed :: Property NoInfo
installed = Apt.installed ["uwsgi"]

restarted :: Property NoInfo
restarted = Service.restarted "uwsgi"

reloaded :: Property NoInfo
reloaded = Service.reloaded "uwsgi"

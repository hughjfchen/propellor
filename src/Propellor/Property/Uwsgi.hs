-- | Maintainer: Félix Sipma <felix+propellor@gueux.org>

module Propellor.Property.Uwsgi where

import Propellor.Base
import qualified Propellor.Property.File as File
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.Service as Service
import System.Posix.Files

type ConfigFile = [String]

type AppName = String

appEnabled :: AppName -> ConfigFile -> RevertableProperty
appEnabled an cf = enable <!> disable
  where
	enable = check test prop
		`describe` ("uwsgi app enabled " ++ an)
		`requires` appAvailable an cf
		`requires` installed
		`onChange` reloaded
	  where
		test = not <$> doesFileExist (appVal an)
		prop = property "uwsgi app in place" $ makeChange $
			createSymbolicLink target dir
		target = appValRelativeCfg an
		dir = appVal an
	disable = trivial $ File.notPresent (appVal an)
		`describe` ("uwsgi app disable" ++ an)
		`requires` installed
		`onChange` reloaded

appAvailable :: AppName -> ConfigFile -> Property NoInfo
appAvailable an cf = ("uwsgi app available " ++ an) ==>
	appCfg an `File.hasContent` (comment : cf)
  where
	comment = "# deployed with propellor, do not modify"

appCfg :: AppName -> FilePath
appCfg an = "/etc/uwsgi/apps-available/" ++ an

appVal :: AppName -> FilePath
appVal an = "/etc/uwsgi/apps-enabled/" ++ an

appValRelativeCfg :: AppName -> FilePath
appValRelativeCfg an = "../apps-available/" ++ an

installed :: Property NoInfo
installed = Apt.installed ["uwsgi"]

restarted :: Property NoInfo
restarted = Service.restarted "uwsgi"

reloaded :: Property NoInfo
reloaded = Service.reloaded "uwsgi"

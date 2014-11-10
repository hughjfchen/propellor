module Propellor.Property.Prosody where

import Propellor
import qualified Propellor.Property.File as File
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.Service as Service
import System.Posix.Files

type ConfigFile = [String]

type Conf = String

confEnabled :: Conf -> ConfigFile -> RevertableProperty
confEnabled conf cf = RevertableProperty enable disable
  where
	enable = check test prop
		`describe` ("prosody conf enabled " ++ conf)
		`requires` confAvailable conf cf
		`requires` installed
		`onChange` reloaded
	  where
		test = not <$> doesFileExist (confValPath conf)
		prop = property "prosody conf in place" $ makeChange $
			createSymbolicLink target dir
		target = confValRelativePath conf
		dir = confValPath conf
		confValRelativePath conf' = "../conf.avail" </> conf' <.> "cfg.lua"
	disable = trivial $ File.notPresent (confValPath conf)
		`describe` ("prosody conf disabled " ++ conf)
		`requires` installed
		`onChange` reloaded

confAvailable :: Conf -> ConfigFile -> Property
confAvailable conf cf = ("prosody conf available " ++ conf) ==>
	confAvailPath conf `File.hasContent` (comment : cf)
  where
	comment = "-- deployed with propellor, do not modify"

confAvailPath :: Conf -> FilePath
confAvailPath conf = "/etc/prosody/conf.avail" </> conf <.> "cfg.lua"

confValPath :: Conf -> FilePath
confValPath conf = "/etc/prosody/conf.d" </> conf <.> "cfg.lua"

installed :: Property
installed = Apt.installed ["prosody"]

restarted :: Property
restarted = Service.restarted "prosody"

reloaded :: Property
reloaded = Service.reloaded "prosody"

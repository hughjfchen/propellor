module Propellor.Property.Tor where

import Propellor
import qualified Propellor.Property.File as File
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.Service as Service

isBridge :: Property
isBridge = setup `requires` Apt.installed ["tor"]
	`describe` "tor bridge"
  where
	setup = mainConfig `File.hasContent`
		[ "SocksPort 0"
		, "ORPort 443"
		, "BridgeRelay 1"
		, "Exitpolicy reject *:*"
		] `onChange` restarted

hiddenServiceAvailable :: HostName -> Int -> Property
hiddenServiceAvailable hn port = hiddenServiceHostName prop
  where
	prop = mainConfig `File.containsLines`
		[ unlines ["HiddenServiceDir", varLib </> hn]
		, unlines ["HiddenServicePort", show port, "127.0.0.1:" ++ show port]
		]
		`describe` "hidden service available"
		`onChange` Service.reloaded "tor"
	hiddenServiceHostName p =  adjustProperty p $ \satisfy -> do
		r <- satisfy
		h <- liftIO $ readFile (varLib </> hn </> "hostname")
		warningMessage $ unlines ["hidden service hostname:", h]
		return r

hiddenService :: HostName -> Int -> Property
hiddenService hn port = mainConfig `File.containsLines`
	[ unlines ["HiddenServiceDir", varLib </> hn]
	, unlines ["HiddenServicePort", show port, "127.0.0.1:" ++ show port]
	]
	`describe` unlines ["hidden service available:", hn, show port]
	`onChange` restarted

restarted :: Property
restarted = Service.restarted "tor"

mainConfig :: FilePath
mainConfig = "/etc/tor/torrc"

varLib :: FilePath
varLib = "/var/lib/tor"

varRun :: FilePath
varRun = "/var/run/tor"

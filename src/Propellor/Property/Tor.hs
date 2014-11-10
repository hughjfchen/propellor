module Propellor.Property.Tor where

import Propellor
import qualified Propellor.Property.File as File
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.Service as Service

isBridge :: Property
isBridge = setup `requires` Apt.installed ["tor"]
	`describe` "tor bridge"
  where
	setup = "/etc/tor/torrc" `File.hasContent`
		[ "SocksPort 0"
		, "ORPort 443"
		, "BridgeRelay 1"
		, "Exitpolicy reject *:*"
		] `onChange` restarted

hiddenServiceAvailable :: FilePath -> Int -> Property
hiddenServiceAvailable dir port = hiddenServiceHostName prop
  where
	prop = "/etc/tor/torrc" `File.containsLines`
			[ "HiddenServiceDir " ++ dir
			, "HiddenServicePort " ++ show port ++ " 127.0.0.1:" ++ show port
			]
			`describe` "hidden service available"
			`onChange` Service.reloaded "tor"
	hiddenServiceHostName p =  adjustProperty p $ \satisfy -> do
		r <- satisfy
		h <- liftIO $ readFile (dir </> "hostname")
		warningMessage $ unlines ["hidden service hostname:", h]
		return r

hiddenService :: FilePath -> Int -> Property
hiddenService dir port = "/etc/tor/torrc" `File.containsLines`
	[ "HiddenServiceDir " ++ dir
	, "HiddenServicePort " ++ show port ++ " 127.0.0.1:" ++ show port
	]
	`describe` ("hidden service (" ++ dir ++ " " ++ show port ++ ") available")
	`onChange` Service.reloaded "tor"

restarted :: Property
restarted = Service.restarted "tor"

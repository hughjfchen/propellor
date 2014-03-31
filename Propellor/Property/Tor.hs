module Propellor.Property.Tor where

import Propellor
import qualified Propellor.Property.File as File
import qualified Propellor.Property.Apt as Apt

isBridge :: Property
isBridge = setup `requires` Apt.installed ["tor"]
	`describe` "tor bridge"
  where
	setup = "/etc/tor/torrc" `File.hasContent`
		[ "SocksPort 0"
		, "ORPort 443"
		, "BridgeRelay 1"
		, "Exitpolicy reject *:*"
		] `onChange` restartTor

restartTor :: Property
restartTor = cmdProperty "service" ["tor", "restart"]

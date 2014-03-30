module Property.Tor where

import Property
import Utility.SafeCommand
import qualified Property.Apt as Apt

isBridge :: Property
isBridge = setup `requires` Apt.installed ["tor"]
  where
	setup = fileHasContent "/etc/tor/torrc"
		[ "SocksPort 0"
		, "ORPort 443"
		, "BridgeRelay 1"
		, "Exitpolicy reject *:*"
		] `onChange` restartTor

restartTor :: Property
restartTor = cmdProperty "service" [Param "tor", Param "restart"]

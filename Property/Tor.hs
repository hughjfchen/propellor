module Property.Tor where

import Common
import qualified Property.File as File
import qualified Property.Apt as Apt

isBridge :: Property
isBridge = setup `requires` Apt.installed ["tor"]
  where
	setup = "/etc/tor/torrc" `File.hasContent`
		[ "SocksPort 0"
		, "ORPort 443"
		, "BridgeRelay 1"
		, "Exitpolicy reject *:*"
		] `onChange` restartTor

restartTor :: Property
restartTor = cmdProperty "service" [Param "tor", Param "restart"]

module Property.Tor where

import Control.Applicative
import Control.Monad
import System.FilePath

import Property
import Property.User
import Utility.SafeCommand
import Utility.Exception

isBridge :: Property
isBridge = fileHasContent "/etc/tor/torrc"
	[ "SocksPort 0"
	, "ORPort 443"
	, "BridgeRelay 1"
	, "Exitpolicy reject *:*"
	] `onChange` restartTor

restartTor :: Property
restartTor = cmdProperty "service" [Param "tor", Param "restart"]

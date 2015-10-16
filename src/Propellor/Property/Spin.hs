{-# LANGUAGE FlexibleInstances #-}

module Propellor.Property.Spin (Controlled(..), controller) where

import Propellor.Base
import Propellor.Spin (spin)
import Propellor.Types.CmdLine (ControllerChain(..))
import Propellor.Types.Info

class Controlled t where
	toHosts :: t -> [Host]

instance Controlled Host where
	toHosts h = [h]

instance Controlled [Host] where
	toHosts = id

-- | The Host that has this Property is in control of some other Hosts.
--
-- Making a host a controller eliminates the need to manually run
-- propellor --spin to update the controlled hosts. Each time
-- propellor is run on the controller host, it will in turn run
-- propellor on the controlled Hosts.
--
-- Multiple controllers can control the same hosts. However, if
-- propellor is already running on a host, its controller will fail
-- to run it a second time. So, if two controllers both try to
-- control the same host at the same time, one will fail.
--
-- Chains of controllers are supported; host A can control host B which
-- controls host C. Loops of controllers are automatically prevented.
controller :: Controlled h => h -> Property NoInfo
controller h = propertyList "controller" (map controller' (toHosts h))

controller' :: Host -> Property NoInfo
controller' h = property ("controller for " ++ hostName h) $ do
	ControllerChain cc <- getControllerChain
	if hostName h `elem` cc
		then noChange -- avoid loop
		else do
			liftIO $ spin (hostName h) Nothing (ControllerChain cc) h
			-- Don't know if the spin made a change to the
			-- remote host or not, but in any case, the
			-- local host was not changed.
			noChange

getControllerChain :: Propellor ControllerChain
getControllerChain = do
	hn <- hostName <$> ask
	ControllerChain cc <- fromMaybe (ControllerChain []) . fromInfoVal <$> askInfo
	return (ControllerChain (hn:cc))

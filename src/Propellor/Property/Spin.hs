{-# LANGUAGE FlexibleInstances #-}

module Propellor.Property.Spin (Spinnable(..), controller) where

import Propellor.Base
import Propellor.Spin (spin)
import Propellor.Types.CmdLine (ControllerChain(..))
import Propellor.Types.Info

-- | A class of things that can be spinned.
class Spinnable t where
	toSpin :: t -> Property NoInfo

instance Spinnable Host where
	toSpin h = property (cdesc (hostName h)) $ do
		ControllerChain cc <- getControllerChain
		if hostName h `elem` cc
			then noChange -- avoid loop
			else do
				liftIO $ spin (hostName h) Nothing (ControllerChain cc) h
				-- Don't know if the spin made a change to the
				-- remote host or not, but in any case, the
				-- local host was not changed.
				noChange

-- | Each Host in the list is spinned in turn. Does not stop on spin
-- failure; does propigate overall success/failure.
instance Spinnable [Host] where
	toSpin l = propertyList (cdesc $ unwords $ map hostName l) (map toSpin l)

-- | The Host that has this Property is in control of running propellor on
-- some other Hosts.
--
-- Making a host a controller eliminates the need to manually run
-- propellor --spin to update the controlled hosts. Each time
-- propellor is run on the controller host, it will in turn run
-- propellor on the controlled Hosts.
--
-- For example, if you have some webservers and some dnsservers,
-- and want a master that runs propellor on all of them:
--
-- > import Propellor
-- > import qualified Propellor.Property.Spin as Spin
-- > import qualified Propellor.Property.Cron as Cron
-- > 
-- > main = defaultMain hosts
-- >
-- > hosts = master : webservers ++ dnsservers
-- > 
-- > webservers = ...
-- > 
-- > dnsservers = ...
-- > 
-- > master = host "master.example.com"
-- >	& Cron.runPropellor
-- > 	-- Only update dnsservers once all webservers are successfully updated.
-- >	& Spin.controller dnsservers
-- >		`requires` Spin.controller webservers
--
-- Multiple controllers can control the same hosts. However, when
-- propellor is already running on a host, a controller will fail
-- to run it. So, if two controllers both try to control the same
-- host at the same time, one will fail.
--
-- Chains of controllers are supported; host A can control host B which
-- controls host C. Loops of controllers are automatically prevented.
controller :: Spinnable h => h -> Property NoInfo
controller = toSpin

cdesc :: String -> Desc
cdesc n = "controller for " ++ n

getControllerChain :: Propellor ControllerChain
getControllerChain = do
	hn <- hostName <$> ask
	ControllerChain cc <- fromMaybe (ControllerChain []) . fromInfoVal <$> askInfo
	return (ControllerChain (hn:cc))

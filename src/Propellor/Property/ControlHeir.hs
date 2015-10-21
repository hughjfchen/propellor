{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving #-}

module Propellor.Property.ControlHeir (
	ControlHeir(..),
	ControlList(..),
	addControlHeir,
	ControllerOf(..),
) where

import Propellor.Base
import Propellor.Spin (spin, SpinMode(..))
import Propellor.Types.Info
import qualified Propellor.Property.Ssh as Ssh

-- | A hierarchy of control. When propellor is run on a host that
-- is a Controller, it in turn spins each of the hosts in its control
-- list.
--
-- There can be multiple levels of controllers in the hierarchy.
--
-- Multiple controllers can control the same hosts. However, when
-- propellor is already running on a host, a controller will fail
-- to spin it. So, if two controllers both try to control the same
-- host at the same time, one will fail.
-- 
-- (Loops in the hierarchy, such as a host controlling itself,
-- are detected and automatically broken.)
data ControlHeir
	= Controller Host ControlList
	| Controlled Host

instance Show ControlHeir where
	show (Controller h l) = "Controller " ++ hostName h ++ " (" ++ show l ++ ")"
	show (Controlled h) = "Controlled " ++ hostName h

data ControlList
	-- | A list of hosts to control. Failure to spin one host does not
	-- prevent spinning later hosts in the list.
	= ControlList [ControlHeir]
	-- | Requires the first host to be successfully spinned before
	-- proceeding to spin the hosts in the ControlList.
	| ControlReq ControlHeir ControlList
	deriving (Show)

listHeir :: ControlList -> [ControlHeir]
listHeir (ControlList l) = l
listHeir (ControlReq h l) = h : listHeir l

class DirectlyControlled a where
	directlyControlled :: a -> [Host]

instance DirectlyControlled ControlHeir where
	directlyControlled (Controlled h) = [h]
	directlyControlled (Controller h _) = [h]

instance DirectlyControlled ControlList where
	directlyControlled = concatMap directlyControlled . listHeir

-- Removes any loops that may be present in the ControlHeir involving
-- the passed Host. This is a simple matter of removing the Host from any
-- sub-hierarchies.
deloop :: Host -> ControlHeir -> ControlHeir
deloop _ (Controlled h) = Controlled h
deloop thehost (Controller h cl) = Controller h (removeh cl)
  where
	removeh (ControlList l) = ControlList (mapMaybe removeh' l)
	removeh (ControlReq ch cl') = case removeh' ch of
		Just ch' -> ControlReq ch' (removeh cl')
		Nothing -> removeh cl'
	removeh' (Controlled h')
		| hostName h' == hostName thehost = Nothing
		| otherwise = Just (Controlled h')
	removeh' (Controller h' cl')
		| hostName h' == hostName thehost = Nothing
		| otherwise = Just (Controller h' (removeh cl'))

-- | Applies a ControlHeir to a list of hosts.
--
-- This eliminates the need to manually run propellor --spin to
-- update the controlled hosts. Each time propellor is run
-- on the controller host, it will in turn run propellor
-- on each of the controlled Hosts.
--
-- The controller needs to be able to ssh to the hosts it controls,
-- and run propellor, as root. To this end, 
-- the `Propellor.Property.Ssh.knownHost` property is added to the
-- controller, so it knows the host keys of the hosts it controlls.
--
-- Each controlled host is configured to let its controller
-- ssh in as root. This is done by adding the 
-- `Propellor.Property.Ssh.authorizedKeysFrom` property, with
-- `User "root"`.
--
-- It's left up to you to use `Propellor.Property.Ssh.userKeys` to
-- configure the ssh keys for the root user on controller hosts,
-- and to use `Ssh.hostKeys` to configure the host keys for the controlled
-- hosts.
--
-- For example, if you have some webservers and a dnsserver,
-- and want a master that runs propellor on all of them:
--
-- > import Propellor
-- > import Propellor.Property.ControlHeir
-- > import qualified Propellor.Property.Ssh as Ssh
-- > import qualified Propellor.Property.Cron as Cron
-- > 
-- > main = defaultMain (hosts `addControlHeir` control)
-- >
-- > hosts =
-- > 	[ master
-- >	, dnsserver
-- >	] ++ webservers
-- > 
-- > control = Controller master (ControlList (map Controlled (dnsserver:webservers)))
-- >
-- > dnsserver = host "dns.example.com"
-- >	& Ssh.hostKeys hostContext [(SshEd25519, "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIB3BJ2GqZiTR2LEoDXyYFgh/BduWefjdKXAsAtzS9zeI")]
-- >	& ...
-- > 
-- > webservers =
-- >    [ host "www1.example.com"
-- >		& Ssh.hostKeys hostContext [(SshEd25519, "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICfFntnesZcYz2B2T41ay45igfckXRSh5uVffkuCQkLv")]
-- >		& ...
-- >	, ...
-- >	]
-- >
-- > master = host "master.example.com"
-- >	& Ssh.userKeys (User "root") [(SshEd25519, "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFWD0Hau5FDLeNrDHKilNMKm9c68R3WD+NJOp2jPWvJV")]
-- >	& Cron.runPropellor
--
-- Note that a controller can see all PrivData of the hosts below it in
-- the ControlHeir.
addControlHeir :: [Host] -> ControlHeir -> [Host]
addControlHeir hs (Controlled _) = hs
addControlHeir hs c@(Controller _ _)
	| any isController hs = error "Detected repeated applications of addControlHeir. Since loop prevention only works within a single application, repeated application is unsafe and not allowed."
	| otherwise = map (\h -> addControlHeir' h (deloop h c)) hs

-- Walk through the ControlHeir, and add properties to the Host
-- depending on where it appears in the ControlHeir.
-- (Loops are already removed before this point.)
addControlHeir' :: Host -> ControlHeir -> Host
addControlHeir' h (Controlled _) = h
addControlHeir' h (Controller controller l)
	| hn == hostName controller = cont $
		h & mkcontroller l
	| hn `elem` map hostName (directlyControlled l) = cont $
		h & controlledBy controller
	| otherwise = cont h
  where
	hn = hostName h

	cont h' = foldl addControlHeir' h' (listHeir l)
	
	mkcontroller (ControlList l') =
		mkcontroller' (concatMap directlyControlled l')
	mkcontroller (ControlReq h' l') =
		mkcontroller' (directlyControlled h')
			`before` mkcontroller l' 
	mkcontroller' l' = propertyList
		(cdesc $ unwords $ map hostName l')
		(map controllerFor l')

-- | The host this property is added to becomes the controller for the
-- specified Host.
controllerFor :: Host -> Property HasInfo
controllerFor h = infoProperty desc go (mkControllingInfo h <> privinfo) []
	`requires` Ssh.knownHost [h] (hostName h) (User "root")
	`requires` Ssh.installed
  where
	desc = cdesc (hostName h)

	go = do
		liftIO $ spin ControllingSpin (hostName h) h
		-- Don't know if the spin made a change to
		-- the remote host or not, but in any case,
		-- the local host was not changed.
		noChange

	-- Make the controlling host have all the remote host's
	-- PrivData, so it can send it on to the remote host
	-- when spinning it.
	privinfo = addInfo mempty $
		forceHostContext (hostName h) $
			getInfo (hostInfo h)

-- | Use this property to let the specified controller Host ssh in
-- and run propellor.
controlledBy :: Host -> Property NoInfo
controlledBy h = User "root" `Ssh.authorizedKeysFrom` (User "root", h)
	`requires` Ssh.installed

cdesc :: String -> Desc
cdesc n = "controller for " ++ n

-- | A Host's Info contains a list of the names of hosts it's controlling.
newtype ControllerOf = ControllerOf [HostName]
	deriving (Typeable, Monoid, Show)

instance IsInfo ControllerOf where
	propagateInfo _ = True

mkControllingInfo :: Host -> Info
mkControllingInfo controlled = addInfo mempty (ControllerOf [hostName controlled])

isController :: Host -> Bool
isController h = case getInfo (hostInfo h) of
	ControllerOf [] -> False
	_ -> True

-- |Properties for configuring firewall (iptables) rules
--
-- Copyright 2014 Arnaud Bailly <arnaud.oqube@gmail.com>
-- License: BSD-2-Clause
module Propellor.Property.Firewall (
	rule,
	installed,
	Chain(..),
	Target(..),
	Proto(..),
	Rules(..),
	ConnectionState(..)
) where

import Data.Monoid
import Data.Char
import Data.List

import Propellor
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.Network as Network

installed :: Property NoInfo
installed = Apt.installed ["iptables"]

rule :: Chain -> Target -> Rules -> Property NoInfo
rule c t rs = property ("firewall rule: " <> show r) addIpTable
  where
	r = Rule c t rs
	addIpTable = liftIO $ do
		let args = toIpTable r
		exist <- boolSystem "iptables" (chk args)
		if exist
			then return NoChange
			else toResult <$> boolSystem "iptables" (add args)
	add params = (Param "-A") : params
	chk params = (Param "-C") : params

toIpTable :: Rule -> [CommandParam]
toIpTable r =  map Param $
	(show $ ruleChain r) :
	(toIpTableArg (ruleRules r)) ++ [ "-j" , show $ ruleTarget r ]

toIpTableArg :: Rules -> [String]
toIpTableArg Everything        = []
toIpTableArg (Proto proto)     = ["-p", map toLower $ show proto]
toIpTableArg (DPort port)       = ["--dport", show port]
toIpTableArg (DPortRange (f,t)) = ["--dport", show f ++ ":" ++ show t]
toIpTableArg (IFace iface)     = ["-i", iface]
toIpTableArg (Ctstate states)  = ["-m", "conntrack","--ctstate", concat $ intersperse "," (map show states)]
toIpTableArg (r :- r')         = toIpTableArg r <> toIpTableArg r'

data Rule = Rule
	{ ruleChain :: Chain
	, ruleTarget :: Target
	, ruleRules :: Rules
	} deriving (Eq, Show)

data Chain = INPUT | OUTPUT | FORWARD
	deriving (Eq, Show)

data Target = ACCEPT | REJECT | DROP | LOG
	deriving (Eq, Show)

data Proto = TCP | UDP | ICMP
	deriving (Eq, Show)

data ConnectionState = ESTABLISHED | RELATED | NEW | INVALID
	deriving (Eq, Show)

data Rules
	= Everything
	| Proto Proto
	-- ^There is actually some order dependency between proto and port so this should be a specific
	-- data type with proto + ports
	| DPort Port
	| DPortRange (Port,Port)
	| IFace Network.Interface
	| Ctstate [ ConnectionState ]
	| Rules :- Rules   -- ^Combine two rules
	deriving (Eq, Show)

infixl 0 :-

instance Monoid Rules where
	mempty  = Everything
	mappend = (:-)

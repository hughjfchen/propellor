-- | Maintainer: Arnaud Bailly <arnaud.oqube@gmail.com>
--
-- Properties for configuring firewall (iptables) rules

module Propellor.Property.Firewall (
	rule,
	installed,
	Chain(..),
	Table(..),
	TargetFilter(..),
	TargetNat(..),
	TargetMangle(..),
	TargetRaw(..),
	TargetSecurity(..),
	Proto(..),
	Rules(..),
	ConnectionState(..),
	IPWithMask(..),
	fromIPWithMask
) where

import Data.Monoid
import Data.Char
import Data.List

import Propellor.Base
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.Network as Network

installed :: Property NoInfo
installed = Apt.installed ["iptables"]

rule :: Chain -> Table -> Rules -> Property NoInfo
rule c t rs = property ("firewall rule: " <> show r) addIpTable
  where
	r = Rule c t rs
	addIpTable = liftIO $ do
		let args = toIpTable r
		exist <- boolSystem "iptables" (chk args)
		if exist
			then return NoChange
			else toResult <$> boolSystem "iptables" (add args)
	add params = Param "-A" : params
	chk params = Param "-C" : params

toIpTable :: Rule -> [CommandParam]
toIpTable r =  map Param $
	show (ruleChain r) :
	toIpTableArg (ruleRules r) ++ toIpTableTable (ruleTable r)

toIpTableArg :: Rules -> [String]
toIpTableArg Everything = []
toIpTableArg (Proto proto) = ["-p", map toLower $ show proto]
toIpTableArg (DPort (Port port)) = ["--dport", show port]
toIpTableArg (DPortRange (Port f, Port t)) =
	["--dport", show f ++ ":" ++ show t]
toIpTableArg (InIFace iface) = ["-i", iface]
toIpTableArg (OutIFace iface) = ["-o", iface]
toIpTableArg (Ctstate states) =
	[ "-m"
	, "conntrack"
	, "--ctstate", intercalate "," (map show states)
	]
toIpTableArg (Source ipwm) =
	[ "-s"
	, intercalate "," (map fromIPWithMask ipwm)
	]
toIpTableArg (Destination ipwm) =
	[ "-d"
	, intercalate "," (map fromIPWithMask ipwm)
	]
toIpTableArg (r :- r') = toIpTableArg r <> toIpTableArg r'

data IPWithMask = IPWithNoMask IPAddr | IPWithIPMask IPAddr IPAddr | IPWithNumMask IPAddr Int
	deriving (Eq, Show)

fromIPWithMask :: IPWithMask -> String
fromIPWithMask (IPWithNoMask ip) = fromIPAddr ip
fromIPWithMask (IPWithIPMask ip ipm) = fromIPAddr ip ++ "/" ++ fromIPAddr ipm
fromIPWithMask (IPWithNumMask ip m) = fromIPAddr ip ++ "/" ++ show m

data Rule = Rule
	{ ruleChain :: Chain
	, ruleTable :: Table
	, ruleRules :: Rules
	} deriving (Eq, Show)

data Table = Filter TargetFilter | Nat TargetNat | Mangle TargetMangle | Raw TargetRaw | Security TargetSecurity
	deriving (Eq, Show)

toIpTableTable :: Table -> [String]
toIpTableTable f = ["-t", table, "-j", target]
  where
	(table, target) = toIpTableTable' f

toIpTableTable' :: Table -> (String, String)
toIpTableTable' (Filter target) = ("filter", fromTarget target)
toIpTableTable' (Nat target) = ("nat", fromTarget target)
toIpTableTable' (Mangle target) = ("mangle", fromTarget target)
toIpTableTable' (Raw target) = ("raw", fromTarget target)
toIpTableTable' (Security target) = ("security", fromTarget target)

data Chain = INPUT | OUTPUT | FORWARD
	deriving (Eq, Show)

data TargetFilter = ACCEPT | REJECT | DROP | LOG | FilterCustom String
	deriving (Eq, Show)

class FromTarget a where
	fromTarget :: a -> String

instance FromTarget TargetFilter where
	fromTarget ACCEPT = "ACCEPT"
	fromTarget REJECT = "REJECT"
	fromTarget DROP = "DROP"
	fromTarget LOG = "LOG"
	fromTarget (FilterCustom f) = f

data TargetNat = NatPREROUTING | NatOUTPUT | NatPOSTROUTING | NatCustom String
	deriving (Eq, Show)

instance FromTarget TargetNat where
	fromTarget NatPREROUTING = "PREROUTING"
	fromTarget NatOUTPUT = "OUTPUT"
	fromTarget NatPOSTROUTING = "POSTROUTING"
	fromTarget (NatCustom f) = f

data TargetMangle = ManglePREROUTING | MangleOUTPUT | MangleINPUT | MangleFORWARD | ManglePOSTROUTING | MangleCustom String
	deriving (Eq, Show)

instance FromTarget TargetMangle where
	fromTarget ManglePREROUTING = "PREROUTING"
	fromTarget MangleOUTPUT = "OUTPUT"
	fromTarget MangleINPUT = "INPUT"
	fromTarget MangleFORWARD = "FORWARD"
	fromTarget ManglePOSTROUTING = "POSTROUTING"
	fromTarget (MangleCustom f) = f

data TargetRaw = RawPREROUTING | RawOUTPUT | RawCustom String
	deriving (Eq, Show)

instance FromTarget TargetRaw where
	fromTarget RawPREROUTING = "PREROUTING"
	fromTarget RawOUTPUT = "OUTPUT"
	fromTarget (RawCustom f) = f

data TargetSecurity = SecurityINPUT | SecurityOUTPUT | SecurityFORWARD | SecurityCustom String
	deriving (Eq, Show)

instance FromTarget TargetSecurity where
	fromTarget SecurityINPUT = "INPUT"
	fromTarget SecurityOUTPUT = "OUTPUT"
	fromTarget SecurityFORWARD = "FORWARD"
	fromTarget (SecurityCustom f) = f

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
	| InIFace Network.Interface
	| OutIFace Network.Interface
	| Ctstate [ ConnectionState ]
	| Source [ IPWithMask ]
	| Destination [ IPWithMask ]
	| Rules :- Rules   -- ^Combine two rules
	deriving (Eq, Show)

infixl 0 :-

instance Monoid Rules where
	mempty  = Everything
	mappend = (:-)

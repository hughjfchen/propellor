-- | Maintainer: Arnaud Bailly <arnaud.oqube@gmail.com>
--
-- Properties for configuring firewall (iptables) rules

module Propellor.Property.Firewall (
	rule,
	installed,
	Chain(..),
	Table(..),
	Target(..),
	Proto(..),
	Rules(..),
	ConnectionState(..),
	ICMPTypeMatch(..),
	Frequency(..),
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

rule :: Chain -> Table -> Target -> Rules -> Property NoInfo
rule c tb tg rs = property ("firewall rule: " <> show r) addIpTable
  where
	r = Rule c tb tg rs
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
	fromChain (ruleChain r) :
	toIpTableArg (ruleRules r) ++
	["-t", fromTable (ruleTable r), "-j", fromTarget (ruleTarget r)]

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
toIpTableArg (ICMPType i) =
	[ "-m"
	, "icmp"
	, "--icmp-type", fromICMPTypeMatch i
	]
toIpTableArg (RateLimit f) =
	[ "-m"
	, "limit"
	, "--limit", fromFrequency f
	]
toIpTableArg (TCPFlags m c) =
	[ "-m"
	, "tcp"
	, "--tcp-flags"
	, intercalate "," (map show m)
	, intercalate "," (map show c)
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
	{ ruleChain  :: Chain
	, ruleTable  :: Table
	, ruleTarget :: Target
	, ruleRules  :: Rules
	} deriving (Eq, Show)

data Table = Filter | Nat | Mangle | Raw | Security
	deriving (Eq, Show)

fromTable :: Table -> String
fromTable Filter = "filter"
fromTable Nat = "nat"
fromTable Mangle = "mangle"
fromTable Raw = "raw"
fromTable Security = "security"

data Target = ACCEPT | REJECT | DROP | LOG | TargetCustom String
	deriving (Eq, Show)

fromTarget :: Target -> String
fromTarget ACCEPT = "ACCEPT"
fromTarget REJECT = "REJECT"
fromTarget DROP = "DROP"
fromTarget LOG = "LOG"
fromTarget (TargetCustom t) = t

data Chain = ChainFilter | ChainNat | ChainMangle | ChainRaw | ChainSecurity
	deriving (Eq, Show)

instance FromChain Chain where
	fromChain = fromChain

class FromChain a where
	fromChain :: a -> String

data ChainFilter = INPUT | OUTPUT | FORWARD | FilterCustom String
	deriving (Eq, Show)

instance FromChain ChainFilter where
	fromChain INPUT = "INPUT"
	fromChain OUTPUT = "OUTPUT"
	fromChain FORWARD = "FORWARD"
	fromChain (FilterCustom c) = c

data ChainNat = NatPREROUTING | NatOUTPUT | NatPOSTROUTING | NatCustom String
	deriving (Eq, Show)

instance FromChain ChainNat where
	fromChain NatPREROUTING = "PREROUTING"
	fromChain NatOUTPUT = "OUTPUT"
	fromChain NatPOSTROUTING = "POSTROUTING"
	fromChain (NatCustom f) = f

data ChainMangle = ManglePREROUTING | MangleOUTPUT | MangleINPUT | MangleFORWARD | ManglePOSTROUTING | MangleCustom String
	deriving (Eq, Show)

instance FromChain ChainMangle where
	fromChain ManglePREROUTING = "PREROUTING"
	fromChain MangleOUTPUT = "OUTPUT"
	fromChain MangleINPUT = "INPUT"
	fromChain MangleFORWARD = "FORWARD"
	fromChain ManglePOSTROUTING = "POSTROUTING"
	fromChain (MangleCustom f) = f

data ChainRaw = RawPREROUTING | RawOUTPUT | RawCustom String
	deriving (Eq, Show)

instance FromChain ChainRaw where
	fromChain RawPREROUTING = "PREROUTING"
	fromChain RawOUTPUT = "OUTPUT"
	fromChain (RawCustom f) = f

data ChainSecurity = SecurityINPUT | SecurityOUTPUT | SecurityFORWARD | SecurityCustom String
	deriving (Eq, Show)

instance FromChain ChainSecurity where
	fromChain SecurityINPUT = "INPUT"
	fromChain SecurityOUTPUT = "OUTPUT"
	fromChain SecurityFORWARD = "FORWARD"
	fromChain (SecurityCustom f) = f

data Proto = TCP | UDP | ICMP
	deriving (Eq, Show)

data ConnectionState = ESTABLISHED | RELATED | NEW | INVALID
	deriving (Eq, Show)

data ICMPTypeMatch = ICMPTypeName String | ICMPTypeCode Int
	deriving (Eq, Show)

fromICMPTypeMatch :: ICMPTypeMatch -> String
fromICMPTypeMatch (ICMPTypeName t) = t
fromICMPTypeMatch (ICMPTypeCode c) = show c

data Frequency = NumBySecond Int
	deriving (Eq, Show)

fromFrequency :: Frequency -> String
fromFrequency (NumBySecond n) = show n ++ "/second"

type TCPFlagMask = [TCPFlag]

type TCPFlagComp = [TCPFlag]

data TCPFlag = SYN | ACK | FIN | RST | URG | PSH | ALL | NONE
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
	| ICMPType ICMPTypeMatch
	| RateLimit Frequency
	| TCPFlags TCPFlagMask TCPFlagComp
	| Source [ IPWithMask ]
	| Destination [ IPWithMask ]
	| Rules :- Rules   -- ^Combine two rules
	deriving (Eq, Show)

infixl 0 :-

instance Monoid Rules where
	mempty  = Everything
	mappend = (:-)

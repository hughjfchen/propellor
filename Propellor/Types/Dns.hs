module Propellor.Types.Dns where

import Data.Word

type Domain = String

data IPAddr = IPv4 String | IPv6 String
	deriving (Read, Show, Eq, Ord)

fromIPAddr :: IPAddr -> String
fromIPAddr (IPv4 addr) = addr
fromIPAddr (IPv6 addr) = addr

-- | Represents a bind 9 named.conf file.
data NamedConf = NamedConf
	{ confDomain :: Domain
	, confType :: Type
	, confFile :: FilePath
	, confMasters :: [IPAddr]
	, confLines :: [String]
	}
	deriving (Show, Eq)

data Type = Master | Secondary
	deriving (Show, Eq)

-- | Represents a bind 9 zone file.
data Zone = Zone
	{ zDomain :: Domain
	, zSOA :: SOA
	, zHosts :: [(BindDomain, Record)]
	}
	deriving (Read, Show, Eq)

-- | Every domain has a SOA record, which is big and complicated.
data SOA = SOA
	{ sDomain :: BindDomain
	-- ^ Typically ns1.your.domain
	, sSerial :: SerialNumber
	-- ^ The most important parameter is the serial number,
	-- which must increase after each change.
	, sRefresh :: Integer
	, sRetry :: Integer
	, sExpire :: Integer
	, sNegativeCacheTTL :: Integer
	, sRecord :: [Record]
	-- ^ Records for the root of the domain. Typically NS, A, TXT
	}
	deriving (Read, Show, Eq)

-- | Types of DNS records.
--
-- This is not a complete list, more can be added.
data Record
	= Address IPAddr
	| CNAME BindDomain
	| MX Int BindDomain
	| NS BindDomain
	| TXT String
	deriving (Read, Show, Eq, Ord)

getIPAddr :: Record -> Maybe IPAddr
getIPAddr (Address addr) = Just addr
getIPAddr _ = Nothing

getCNAME :: Record -> Maybe BindDomain
getCNAME (CNAME d) = Just d
getCNAME _ = Nothing

-- | Bind serial numbers are unsigned, 32 bit integers.
type SerialNumber = Word32

-- | Domains in the zone file must end with a period if they are absolute.
--
-- Let's use a type to keep absolute domains straight from relative
-- domains.
--
-- The SOADomain refers to the root SOA record.
data BindDomain = RelDomain Domain | AbsDomain Domain | SOADomain
	deriving (Read, Show, Eq, Ord)

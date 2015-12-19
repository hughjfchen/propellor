{-# LANGUAGE PackageImports #-}

module Propellor.Info where

import Propellor.Types
import Propellor.Types.Info

import "mtl" Control.Monad.Reader
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import Control.Applicative
import Prelude

pureInfoProperty :: (IsInfo v) => Desc -> v -> Property HasInfo
pureInfoProperty desc v = pureInfoProperty' desc (addInfo mempty v)

pureInfoProperty' :: Desc -> Info -> Property HasInfo
pureInfoProperty' desc i = infoProperty ("has " ++ desc) (return NoChange) i mempty

-- | Gets a value from the host's Info.
askInfo :: (IsInfo v) => Propellor v
askInfo = asks (getInfo . hostInfo)

-- | Specifies the operating system of a host.
--
-- This only provides info for other Properties, so they can act
-- conditionally on the os.
os :: System -> Property HasInfo
os system = pureInfoProperty ("Operating " ++ show system) (InfoVal system)

--  Gets the operating system of a host, if it has been specified.
getOS :: Propellor (Maybe System)
getOS = fromInfoVal <$> askInfo

-- | Indidate that a host has an A record in the DNS.
--
-- When propellor is used to deploy a DNS server for a domain,
-- the hosts in the domain are found by looking for these
-- and similar properites.
--
-- When propellor --spin is used to deploy a host, it checks
-- if the host's IP Property matches the DNS. If the DNS is missing or
-- out of date, the host will instead be contacted directly by IP address.
ipv4 :: String -> Property HasInfo
ipv4 = addDNS . Address . IPv4

-- | Indidate that a host has an AAAA record in the DNS.
ipv6 :: String -> Property HasInfo
ipv6 = addDNS . Address . IPv6

-- | Indicates another name for the host in the DNS.
--
-- When the host's ipv4/ipv6 addresses are known, the alias is set up
-- to use their address, rather than using a CNAME. This avoids various
-- problems with CNAMEs, and also means that when multiple hosts have the
-- same alias, a DNS round-robin is automatically set up.
alias :: Domain -> Property HasInfo
alias d = pureInfoProperty' ("alias " ++ d) $ mempty
	`addInfo` toAliasesInfo [d]
	-- A CNAME is added here, but the DNS setup code converts it to an
	-- IP address when that makes sense.
	`addInfo` (toDnsInfo $ S.singleton $ CNAME $ AbsDomain d)

addDNS :: Record -> Property HasInfo
addDNS r = pureInfoProperty (rdesc r) (toDnsInfo (S.singleton r))
  where
	rdesc (CNAME d) = unwords ["alias", ddesc d]
	rdesc (Address (IPv4 addr)) = unwords ["ipv4", addr]
	rdesc (Address (IPv6 addr)) = unwords ["ipv6", addr]
	rdesc (MX n d) = unwords ["MX", show n, ddesc d]
	rdesc (NS d) = unwords ["NS", ddesc d]
	rdesc (TXT s) = unwords ["TXT", s]
	rdesc (SRV x y z d) = unwords ["SRV", show x, show y, show z, ddesc d]
	rdesc (SSHFP x y s) = unwords ["SSHFP", show x, show y, s]
	rdesc (INCLUDE f) = unwords ["$INCLUDE", f]
	rdesc (PTR x) = unwords ["PTR", x]

	ddesc (AbsDomain domain) = domain
	ddesc (RelDomain domain) = domain
	ddesc RootDomain = "@"

hostMap :: [Host] -> M.Map HostName Host
hostMap l = M.fromList $ zip (map hostName l) l

aliasMap :: [Host] -> M.Map HostName Host
aliasMap = M.fromList . concat .
	map (\h -> map (\aka -> (aka, h)) $ fromAliasesInfo $ getInfo $ hostInfo h)

findHost :: [Host] -> HostName -> Maybe Host
findHost l hn = (findHostNoAlias l hn) <|> (findAlias l hn)

findHostNoAlias :: [Host] -> HostName -> Maybe Host
findHostNoAlias l hn = M.lookup hn (hostMap l)

findAlias :: [Host] -> HostName -> Maybe Host
findAlias l hn = M.lookup hn (aliasMap l)

getAddresses :: Info -> [IPAddr]
getAddresses = mapMaybe getIPAddr . S.toList . fromDnsInfo . getInfo

hostAddresses :: HostName -> [Host] -> [IPAddr]
hostAddresses hn hosts = maybe [] (getAddresses . hostInfo) (findHost hosts hn)

addHostInfo ::IsInfo v => Host -> v -> Host
addHostInfo h v = h { hostInfo = addInfo (hostInfo h) v }

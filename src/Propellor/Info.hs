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

pureInfoProperty :: Desc -> Info -> Property 
pureInfoProperty desc = Property ("has " ++ desc) (return NoChange)

askInfo :: (Info -> Val a) -> Propellor (Maybe a)
askInfo f = asks (fromVal . f . hostInfo)

os :: System -> Property
os system = pureInfoProperty ("Operating " ++ show system) $
	mempty { _os = Val system }

getOS :: Propellor (Maybe System)
getOS = askInfo _os

-- | Indidate that a host has an A record in the DNS.
--
-- TODO check at run time if the host really has this address.
-- (Can't change the host's address, but as a sanity check.)
ipv4 :: String -> Property
ipv4 = addDNS . Address . IPv4

-- | Indidate that a host has an AAAA record in the DNS.
ipv6 :: String -> Property
ipv6 = addDNS . Address . IPv6

-- | Indicates another name for the host in the DNS.
--
-- When the host's ipv4/ipv6 addresses are known, the alias is set up
-- to use their address, rather than using a CNAME. This avoids various
-- problems with CNAMEs, and also means that when multiple hosts have the
-- same alias, a DNS round-robin is automatically set up.
alias :: Domain -> Property
alias = addDNS . CNAME . AbsDomain

addDNS :: Record -> Property
addDNS r = pureInfoProperty (rdesc r) $
	mempty { _dns = S.singleton r }
  where
	rdesc (CNAME d) = unwords ["alias", ddesc d]
	rdesc (Address (IPv4 addr)) = unwords ["ipv4", addr]
	rdesc (Address (IPv6 addr)) = unwords ["ipv6", addr]
	rdesc (MX n d) = unwords ["MX", show n, ddesc d]
	rdesc (NS d) = unwords ["NS", ddesc d]
	rdesc (TXT s) = unwords ["TXT", s]
	rdesc (SRV x y z d) = unwords ["SRV", show x, show y, show z, ddesc d]

	ddesc (AbsDomain domain) = domain
	ddesc (RelDomain domain) = domain
	ddesc RootDomain = "@"

sshPubKey :: String -> Property
sshPubKey k = pureInfoProperty ("ssh pubkey known") $
	mempty { _sshPubKey = Val k }

getSshPubKey :: Propellor (Maybe String)
getSshPubKey = askInfo _sshPubKey

hostMap :: [Host] -> M.Map HostName Host
hostMap l = M.fromList $ zip (map hostName l) l 

findHost :: [Host] -> HostName -> Maybe Host
findHost l hn = M.lookup hn (hostMap l)

getAddresses :: Info -> [IPAddr]
getAddresses = mapMaybe getIPAddr . S.toList . _dns

hostAddresses :: HostName -> [Host] -> [IPAddr]
hostAddresses hn hosts = case hostInfo <$> findHost hosts hn of
	Nothing -> []
	Just info -> mapMaybe getIPAddr $ S.toList $ _dns info

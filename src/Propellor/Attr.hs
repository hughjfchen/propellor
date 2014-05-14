{-# LANGUAGE PackageImports #-}

module Propellor.Attr where

import Propellor.Types
import Propellor.Types.Attr

import "mtl" Control.Monad.Reader
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Maybe
import Control.Applicative

pureAttrProperty :: Desc -> SetAttr -> Property 
pureAttrProperty desc = Property ("has " ++ desc) (return NoChange)

hostname :: HostName -> Property
hostname name = pureAttrProperty ("hostname " ++ name) $
	\d -> d { _hostname = name }

getHostName :: Propellor HostName
getHostName = asks _hostname

os :: System -> Property
os system = pureAttrProperty ("Operating " ++ show system) $
	\d -> d { _os = Just system }

getOS :: Propellor (Maybe System)
getOS = asks _os

-- | Indidate that a host has an A record in the DNS.
--
-- TODO check at run time if the host really has this address.
-- (Can't change the host's address, but as a sanity check.)
ipv4 :: String -> Property
ipv4 addr = pureAttrProperty ("ipv4 " ++ addr)
	(addDNS $ Address $ IPv4 addr)

-- | Indidate that a host has an AAAA record in the DNS.
ipv6 :: String -> Property
ipv6 addr = pureAttrProperty ("ipv6 " ++ addr)
	(addDNS $ Address $ IPv6 addr)

-- | Indicates another name for the host in the DNS.
alias :: Domain -> Property
alias domain = pureAttrProperty ("alias " ++ domain)
	(addDNS $ CNAME $ AbsDomain domain)

addDNS :: Record -> SetAttr
addDNS record d = d { _dns = S.insert record (_dns d) }

-- | Adds a DNS NamedConf stanza.
--
-- Note that adding a Master stanza for a domain always overrides an
-- existing Secondary stanza, while a Secondary stanza is only added
-- when there is no existing Master stanza.
addNamedConf :: NamedConf -> SetAttr
addNamedConf conf d = d { _namedconf = new }
  where
	m = _namedconf d
	domain = confDomain conf
	new = case (confDnsServerType conf, confDnsServerType <$> M.lookup domain m) of
		(Secondary, Just Master) -> m
		_  -> M.insert domain conf m

getNamedConf :: Propellor (M.Map Domain NamedConf)
getNamedConf = asks _namedconf

sshPubKey :: String -> Property
sshPubKey k = pureAttrProperty ("ssh pubkey known") $
	\d -> d { _sshPubKey = Just k }

getSshPubKey :: Propellor (Maybe String)
getSshPubKey = asks _sshPubKey

hostnameless :: Attr
hostnameless = newAttr (error "hostname Attr not specified")

hostAttr :: Host -> Attr
hostAttr (Host _ mkattrs) = mkattrs hostnameless

hostProperties :: Host -> [Property]
hostProperties (Host ps _) = ps

hostMap :: [Host] -> M.Map HostName Host
hostMap l = M.fromList $ zip (map (_hostname . hostAttr) l) l 

hostAttrMap :: [Host] -> M.Map HostName Attr
hostAttrMap l = M.fromList $ zip (map _hostname attrs) attrs
  where
	attrs = map hostAttr l

findHost :: [Host] -> HostName -> Maybe Host
findHost l hn = M.lookup hn (hostMap l)

getAddresses :: Attr -> [IPAddr]
getAddresses = mapMaybe getIPAddr . S.toList . _dns

hostAddresses :: HostName -> [Host] -> [IPAddr]
hostAddresses hn hosts = case hostAttr <$> findHost hosts hn of
	Nothing -> []
	Just attr -> mapMaybe getIPAddr $ S.toList $ _dns attr

-- | Lifts an action into a different host.
--
-- For example, `fromHost hosts "otherhost" getSshPubKey`
fromHost :: [Host] -> HostName -> Propellor a -> Propellor (Maybe a)
fromHost l hn getter = case findHost l hn of
	Nothing -> return Nothing
	Just h -> liftIO $ Just <$>
		runReaderT (runWithAttr getter) (hostAttr h)

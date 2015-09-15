-- | Properties for the Unbound caching DNS server

module Propellor.Property.Unbound
	( installed
	, restarted
	, reloaded
	, genAddressNoTtl
	, genAddress
	, genMX
	, genPTR
	, revIP
	, canonical
	, genZoneStatic
	, genZoneTransparent
	) where

import Propellor
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.Service as Service

import Data.List
import Data.String.Utils (split, replace)


installed :: Property NoInfo
installed = Apt.installed ["unbound"]

restarted :: Property NoInfo
restarted = Service.restarted "unbound"

reloaded :: Property NoInfo
reloaded = Service.reloaded "unbound"

dValue :: BindDomain -> String
dValue (RelDomain d) = d
dValue (AbsDomain d) = d ++ "."
dValue (RootDomain) = "@"

genAddressNoTtl :: BindDomain -> IPAddr -> String
genAddressNoTtl dom = genAddress dom Nothing

genAddress :: BindDomain -> Maybe Int -> IPAddr -> String
genAddress dom ttl addr = case addr of
	IPv4 _ -> genAddress' "A" dom ttl addr
	IPv6 _ -> genAddress' "AAAA" dom ttl addr

genAddress' :: String -> BindDomain -> Maybe Int -> IPAddr -> String
genAddress' recordtype dom ttl addr = localData $ dValue dom ++ " " ++ maybe "" (\ttl' -> show ttl' ++ " ") ttl ++ "IN " ++ recordtype ++ " " ++ fromIPAddr addr

genMX :: BindDomain -> BindDomain -> Int -> String
genMX dom dest priority = localData $ dValue dom ++ " " ++ "MX" ++ " " ++ show priority ++ " " ++ dValue dest

genPTR :: BindDomain -> IPAddr -> String
genPTR dom ip = localData $ revIP ip ++ ". " ++ "PTR" ++ " " ++ dValue dom

revIP :: IPAddr -> String
revIP (IPv4 addr) = intercalate "." (reverse $ split "." addr) ++ ".in-addr.arpa"
revIP addr@(IPv6 _) = reverse (intersperse '.' $ replace ":" "" $ fromIPAddr $ canonical addr) ++ ".ip6.arpa"

canonical :: IPAddr -> IPAddr
canonical (IPv4 addr) = IPv4 addr
canonical (IPv6 addr) = IPv6 $ intercalate ":" $ map canonicalGroup $ split ":" $ replaceImplicitGroups addr
  where
	canonicalGroup g = case length g of
		0 -> "0000"
		1 -> "000" ++ g
		2 -> "00" ++ g
		3 -> "0" ++ g
		_ -> g
	emptyGroups n = iterate (++ ":") "" !! n
	numberOfImplicitGroups a = 8 - length (split ":" $ replace "::" "" a)
	replaceImplicitGroups a = concat $ aux $ split "::" a
	  where
		aux [] = []
		aux (x : xs) = x : emptyGroups (numberOfImplicitGroups a) : xs

localData :: String -> String
localData conf = "    local-data: \"" ++ conf ++ "\""

genZoneStatic :: BindDomain -> String
genZoneStatic dom = localZone (dValue dom) "static"

genZoneTransparent :: BindDomain -> String
genZoneTransparent dom = localZone (dValue dom) "transparent"

localZone :: String -> String -> String
localZone zone confzone = "    local-zone: \"" ++ zone ++ "\" " ++ confzone

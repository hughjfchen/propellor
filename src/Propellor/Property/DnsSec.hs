module Propellor.Property.DnsSec where

import Propellor
import Propellor.Property.File

-- | Puts the DNSSEC key files in place from PrivData.
--
-- signedPrimary uses this, so this property does not normally need to be
-- used directly.
keysInstalled :: Domain -> RevertableProperty
keysInstalled domain = RevertableProperty setup cleanup
  where
	setup = propertyList "DNSSEC keys installed" $
		map installkey keys

	cleanup = propertyList "DNSSEC keys removed" $
		map (notPresent . keyFn domain) keys

	installkey k = (if isPublic k then hasPrivContentExposedFrom else hasPrivContentFrom)
		(keysrc k) (keyFn domain k) (Context domain)

	keys = [ PubZSK, PrivZSK, PubKSK, PrivKSK ]

	keysrc k = PrivDataSource (DnsSec k) $ unwords
		[ "The file with extension"
		, keyExt k
		, " created by running:"
		, if isZoneSigningKey k
			then "dnssec-keygen -a RSASHA256 -b 2048 -n ZONE " ++ domain
			else "dnssec-keygen -f KSK -a RSASHA256 -b 4096 -n ZONE " ++ domain
		]

-- | The file used for a given key.
keyFn :: Domain -> DnsSecKey -> FilePath
keyFn domain k =  "/etc/bind/propellor" </>
	"K" ++ domain ++ "." ++ show k ++ keyExt k

-- | These are the extensions that dnssec-keygen looks for.
keyExt :: DnsSecKey -> String
keyExt k
	| isPublic k = ".key"
	| otherwise = ".private"

isPublic :: DnsSecKey -> Bool
isPublic k = k `elem` [PubZSK, PubKSK]

isZoneSigningKey :: DnsSecKey -> Bool
isZoneSigningKey k = k `elem` [PubZSK, PrivZSK]

module Propellor.Property.DnsSec where

import Propellor
import qualified Propellor.Property.File as File

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
		map (File.notPresent . keyFn domain) keys

	installkey k = writer (keysrc k) (keyFn domain k) (Context domain)
	  where
		writer
			| isPublic k = File.hasPrivContentExposedFrom
			| otherwise = File.hasPrivContentFrom

	keys = [ PubZSK, PrivZSK, PubKSK, PrivKSK ]

	keysrc k = PrivDataSource (DnsSec k) $ unwords
		[ "The file with extension"
		, keyExt k
		, " created by running:"
		, if isZoneSigningKey k
			then "dnssec-keygen -a RSASHA256 -b 2048 -n ZONE " ++ domain
			else "dnssec-keygen -f KSK -a RSASHA256 -b 4096 -n ZONE " ++ domain
		]

-- | Uses dnssec-signzone to sign a domain's zone file.
--
-- signedPrimary uses this, so this property does not normally need to be
-- used directly.
zoneSigned :: Domain -> FilePath -> RevertableProperty
zoneSigned domain zonefile = RevertableProperty setup cleanup
  where
	setup = check needupdate (forceZoneSigned domain zonefile)
		`requires` toProp (keysInstalled domain)
	
	cleanup = combineProperties ("removed signed zone for " ++ domain)
		[ File.notPresent signedzonefile
		, File.notPresent dssetfile
		, toProp (revert (keysInstalled domain))
		]
	
	signedzonefile = dir </> domain ++ ".signed"
	dssetfile = dir </> "-" ++ domain ++ "."
	dir = takeDirectory zonefile

	-- Need to update the signed zone if the zone file
	-- has a newer timestamp.
	needupdate = do
		v <- catchMaybeIO $ getModificationTime signedzonefile
		case v of
			Nothing -> return True
			Just t1 -> do
				t2 <- getModificationTime zonefile
				return (t2 >= t1)

forceZoneSigned :: Domain -> FilePath -> Property
forceZoneSigned domain zonefile = property ("zone signed for " ++ domain) $ liftIO $ do
	salt <- take 16 <$> saltSha1
 	let p = proc "dnssec-signzone"
		[ "-A"
		, "-3", salt
		, "-N", "keep"
		, "-o", domain
		, zonefile
		-- the ordering of these key files does not matter
		, keyFn domain PubZSK  
		, keyFn domain PubKSK
		]
	-- Run in the same directory as the zonefile, so it will 
	-- write the dsset file there.
	(_, _, _, h) <- createProcess $ 
		p { cwd = Just (takeDirectory zonefile) }
	ifM (checkSuccessProcess h)
		( return MadeChange
		, return FailedChange
		)

saltSha1 :: IO String
saltSha1 = readProcess "sh"
	[ "-c"
	, "head -c 1024 /dev/urandom | sha1sum | cut -d ' ' -f 1"
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

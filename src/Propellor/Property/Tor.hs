module Propellor.Property.Tor where

import Propellor
import qualified Propellor.Property.File as File
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.Service as Service
import Utility.FileMode
import Utility.DataUnits

import System.Posix.Files
import Data.Char
import Data.List

type HiddenServiceName = String

type NodeName = String

-- | Sets up a tor bridge. (Not a relay or exit node.)
--
-- Uses port 443
isBridge :: Property NoInfo
isBridge = configured
	[ ("BridgeRelay", "1")
	, ("Exitpolicy", "reject *:*")
	, ("ORPort", "443")
	]
	`describe` "tor bridge"
	`requires` server

-- | Sets up a tor relay.
--
-- Uses port 443
isRelay :: Property NoInfo
isRelay = configured
	[ ("BridgeRelay", "0")
	, ("Exitpolicy", "reject *:*")
	, ("ORPort", "443")
	]
	`describe` "tor relay"
	`requires` server

-- | Makes the tor node be named, with a known private key.
--
-- This can be moved to a different IP without needing to wait to
-- accumulate trust.
named :: NodeName -> Property HasInfo
named n = configured [("Nickname", n')]
	`describe` ("tor node named " ++ n')
	`requires` torPrivKey (Context ("tor " ++ n))
  where
	n' = saneNickname n

torPrivKey :: Context -> Property HasInfo
torPrivKey context = f `File.hasPrivContent` context
	`onChange` File.ownerGroup f user (userGroup user)
	-- install tor first, so the directory exists with right perms
	`requires` Apt.installed ["tor"]
  where
	f = "/var/lib/tor/keys/secret_id_key"

-- | A tor server (bridge, relay, or exit)
-- Don't use if you just want to run tor for personal use.
server :: Property NoInfo
server = configured [("SocksPort", "0")]
	`requires` Apt.installed ["tor", "ntp"]
	`describe` "tor server"

-- | Specifies configuration settings. Any lines in the config file
-- that set other values for the specified settings will be removed,
-- while other settings are left as-is. Tor is restarted when
-- configuration is changed.
configured :: [(String, String)] -> Property NoInfo
configured settings = File.fileProperty "tor configured" go mainConfig
	`onChange` restarted
  where
	ks = map fst settings
	go ls = sort $ map toconfig $
		filter (\(k, _) -> k `notElem` ks) (map fromconfig ls)
		++ settings
	toconfig (k, v) = k ++ " " ++ v
	fromconfig = separate (== ' ')

data BwLimit
	= PerSecond String
	| PerDay String
	| PerMonth String

-- | Limit incoming and outgoing traffic to the specified
-- amount each.
--
-- For example, PerSecond "30 kibibytes" is the minimum limit
-- for a useful relay.
bandwidthRate :: BwLimit -> Property NoInfo
bandwidthRate (PerSecond s) = bandwidthRate' s 1
bandwidthRate (PerDay s) = bandwidthRate' s (24*60*60)
bandwidthRate (PerMonth s) = bandwidthRate' s (31*24*60*60)

bandwidthRate' :: String -> Integer -> Property NoInfo
bandwidthRate' s divby = case readSize dataUnits s of
	Just sz -> let v = show (sz `div` divby) ++ " bytes"
		in configured [("BandwidthRate", v)]
			`describe` ("tor BandwidthRate " ++ v)
	Nothing -> property ("unable to parse " ++ s) noChange

hiddenServiceAvailable :: HiddenServiceName -> Int -> Property NoInfo
hiddenServiceAvailable hn port = hiddenServiceHostName prop
  where
	prop = configured
		[ ("HiddenServiceDir", varLib </> hn)
		, ("HiddenServicePort", unwords [show port, "127.0.0.1:" ++ show port])
		]
		`describe` "hidden service available"
	hiddenServiceHostName p =  adjustPropertySatisfy p $ \satisfy -> do
		r <- satisfy
		h <- liftIO $ readFile (varLib </> hn </> "hostname")
		warningMessage $ unwords ["hidden service hostname:", h]
		return r

hiddenService :: HiddenServiceName -> Int -> Property NoInfo
hiddenService hn port = configured
	[ ("HiddenServiceDir", varLib </> hn)
	, ("HiddenServicePort", unwords [show port, "127.0.0.1:" ++ show port])
	]
	`describe` unwords ["hidden service available:", hn, show port]

hiddenServiceData :: IsContext c => HiddenServiceName -> c -> Property HasInfo
hiddenServiceData hn context = combineProperties desc
	[ installonion "hostname"
	, installonion "private_key"
	]
  where
	desc = unwords ["hidden service data available in", varLib </> hn]
	installonion f = withPrivData (PrivFile $ varLib </> hn </> f) context $ \getcontent ->
		property desc $ getcontent $ install $ varLib </> hn </> f
	install f content = ifM (liftIO $ doesFileExist f)
		( noChange
		, ensureProperties
			[ property desc $ makeChange $ do
				createDirectoryIfMissing True (takeDirectory f)
				writeFileProtected f content
			, File.mode (takeDirectory f) $ combineModes
				[ownerReadMode, ownerWriteMode, ownerExecuteMode]
			, File.ownerGroup (takeDirectory f) user (userGroup user)
			, File.ownerGroup f user (userGroup user)
			]
		)

restarted :: Property NoInfo
restarted = Service.restarted "tor"

mainConfig :: FilePath
mainConfig = "/etc/tor/torrc"

varLib :: FilePath
varLib = "/var/lib/tor"

varRun :: FilePath
varRun = "/var/run/tor"

user :: User
user = User "debian-tor"

type NickName = String

-- | Convert String to a valid tor NickName.
saneNickname :: String -> NickName
saneNickname s 
	| null n = "unnamed"
	| otherwise = n
  where
	legal c = isNumber c || isAsciiUpper c || isAsciiLower c
	n = take 19 $ filter legal s

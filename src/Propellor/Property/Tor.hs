module Propellor.Property.Tor where

import Propellor
import qualified Propellor.Property.File as File
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.Service as Service
import Utility.FileMode

import System.Posix.Files

type HiddenServiceName = String

isBridge :: Property NoInfo
isBridge = setup `requires` Apt.installed ["tor"]
	`describe` "tor bridge"
  where
	setup = mainConfig `File.hasContent`
		[ "SocksPort 0"
		, "ORPort 443"
		, "BridgeRelay 1"
		, "Exitpolicy reject *:*"
		] `onChange` restarted

hiddenServiceAvailable :: HiddenServiceName -> Int -> Property NoInfo
hiddenServiceAvailable hn port = hiddenServiceHostName prop
  where
	prop = mainConfig `File.containsLines`
		[ unwords ["HiddenServiceDir", varLib </> hn]
		, unwords ["HiddenServicePort", show port, "127.0.0.1:" ++ show port]
		]
		`describe` "hidden service available"
		`onChange` Service.reloaded "tor"
	hiddenServiceHostName p =  adjustPropertySatisfy p $ \satisfy -> do
		r <- satisfy
		h <- liftIO $ readFile (varLib </> hn </> "hostname")
		warningMessage $ unwords ["hidden service hostname:", h]
		return r

hiddenService :: HiddenServiceName -> Int -> Property NoInfo
hiddenService hn port = mainConfig `File.containsLines`
	[ unwords ["HiddenServiceDir", varLib </> hn]
	, unwords ["HiddenServicePort", show port, "127.0.0.1:" ++ show port]
	]
	`describe` unwords ["hidden service available:", hn, show port]
	`onChange` restarted

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
			, File.ownerGroup (takeDirectory f) user user
			, File.ownerGroup f user user
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

user :: UserName
user = "debian-tor"

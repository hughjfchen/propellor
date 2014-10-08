module Propellor.Property.Ssh (
	setSshdConfig,
	permitRootLogin,
	passwordAuthentication,
	hasAuthorizedKeys,
	restarted,
	randomHostKeys,
	hostKeys,
	hostKey,
	keyImported,
	knownHost,
	authorizedKeys,
	listenPort
) where

import Propellor
import qualified Propellor.Property.File as File
import qualified Propellor.Property.Service as Service
import Propellor.Property.User
import Utility.SafeCommand
import Utility.FileMode

import System.PosixCompat

sshBool :: Bool -> String
sshBool True = "yes"
sshBool False = "no"

sshdConfig :: FilePath
sshdConfig = "/etc/ssh/sshd_config"

setSshdConfig :: String -> Bool -> Property
setSshdConfig setting allowed = combineProperties "sshd config"
	[ sshdConfig `File.lacksLine` (sshline $ not allowed)
	, sshdConfig `File.containsLine` (sshline allowed)
	]
	`onChange` restarted
	`describe` unwords [ "ssh config:", setting, sshBool allowed ]
  where
	sshline v = setting ++ " " ++ sshBool v

permitRootLogin :: Bool -> Property
permitRootLogin = setSshdConfig "PermitRootLogin"

passwordAuthentication :: Bool -> Property
passwordAuthentication = setSshdConfig "PasswordAuthentication"

dotDir :: UserName -> IO FilePath
dotDir user = do
	h <- homedir user
	return $ h </> ".ssh"

dotFile :: FilePath -> UserName -> IO FilePath
dotFile f user = do
	d <- dotDir user
	return $ d </> f

hasAuthorizedKeys :: UserName -> IO Bool
hasAuthorizedKeys = go <=< dotFile "authorized_keys"
  where
	go f = not . null <$> catchDefaultIO "" (readFile f)

restarted :: Property
restarted = Service.restarted "ssh"

-- | Blows away existing host keys and make new ones.
-- Useful for systems installed from an image that might reuse host keys.
-- A flag file is used to only ever do this once.
randomHostKeys :: Property
randomHostKeys = flagFile prop "/etc/ssh/.unique_host_keys"
	`onChange` restarted
  where
	prop = property "ssh random host keys" $ do
		void $ liftIO $ boolSystem "sh"
			[ Param "-c"
			, Param "rm -f /etc/ssh/ssh_host_*"
			]
		ensureProperty $ scriptProperty 
			[ "DPKG_MAINTSCRIPT_NAME=postinst DPKG_MAINTSCRIPT_PACKAGE=openssh-server /var/lib/dpkg/info/openssh-server.postinst configure" ]

-- | Sets all types of ssh host keys from the privdata.
hostKeys :: Context -> Property
hostKeys ctx = propertyList "known ssh host keys"
	[ hostKey SshDsa ctx
	, hostKey SshRsa ctx
	, hostKey SshEcdsa ctx
	]

-- | Sets a single ssh host key from the privdata.
hostKey :: SshKeyType -> Context -> Property
hostKey keytype context = combineProperties desc
	[ installkey (SshPubKey keytype "")  (install writeFile ".pub")
	, installkey (SshPrivKey keytype "") (install writeFileProtected "")
	]
	`onChange` restarted
  where
	desc = "known ssh host key (" ++ fromKeyType keytype ++ ")"
	installkey p a = withPrivData p context $ \getkey ->
		property desc $ getkey a
	install writer ext key = do
		let f = "/etc/ssh/ssh_host_" ++ fromKeyType keytype ++ "_key" ++ ext
		s <- liftIO $ readFileStrict f
		if s == key
			then noChange
			else makeChange $ writer f key

-- | Sets up a user with a ssh private key and public key pair from the
-- PrivData.
keyImported :: SshKeyType -> UserName -> Context -> Property
keyImported keytype user context = combineProperties desc
	[ installkey (SshPubKey keytype user) (install writeFile ".pub")
	, installkey (SshPrivKey keytype user) (install writeFileProtected "")
	]
  where
	desc = user ++ " has ssh key (" ++ fromKeyType keytype ++ ")"
	installkey p a = withPrivData p context $ \getkey ->
		property desc $ getkey a
	install writer ext key = do
		f <- liftIO $ keyfile ext
		ifM (liftIO $ doesFileExist f)
			( noChange
			, ensureProperties
				[ property desc $ makeChange $ do
					createDirectoryIfMissing True (takeDirectory f)
					writer f key
				, File.ownerGroup f user user
				, File.ownerGroup (takeDirectory f) user user
				]
			)
	keyfile ext = do
		home <- homeDirectory <$> getUserEntryForName user
		return $ home </> ".ssh" </> "id_" ++ fromKeyType keytype ++ ext

fromKeyType :: SshKeyType -> String
fromKeyType SshRsa = "rsa"
fromKeyType SshDsa = "dsa"
fromKeyType SshEcdsa = "ecdsa"
fromKeyType SshEd25519 = "ed25519"

-- | Puts some host's ssh public key into the known_hosts file for a user.
knownHost :: [Host] -> HostName -> UserName -> Property
knownHost hosts hn user = property desc $
	go =<< fromHost hosts hn getSshPubKey
  where
	desc = user ++ " knows ssh key for " ++ hn
	go (Just (Just k)) = do
		f <- liftIO $ dotFile "known_hosts" user
		ensureProperty $ combineProperties desc
			[ File.dirExists (takeDirectory f)
			, f `File.containsLine` (hn ++ " " ++ k)
			, File.ownerGroup f user user
			]
	go _ = do
		warningMessage $ "no configred sshPubKey for " ++ hn
		return FailedChange

-- | Makes a user have authorized_keys from the PrivData
authorizedKeys :: UserName -> Context -> Property
authorizedKeys user context = withPrivData (SshAuthorizedKeys user) context $ \get ->
	property (user ++ " has authorized_keys") $ get $ \v -> do
		f <- liftIO $ dotFile "authorized_keys" user
		liftIO $ do
			createDirectoryIfMissing True (takeDirectory f)
			writeFileProtected f v
		ensureProperties 
			[ File.ownerGroup f user user
			, File.ownerGroup (takeDirectory f) user user
			] 

-- | Makes the ssh server listen on a given port, in addition to any other
-- ports it is configured to listen on.
--
-- Revert to prevent it listening on a particular port.
listenPort :: Int -> RevertableProperty
listenPort port = RevertableProperty enable disable
  where
	portline = "Port " ++ show port
	enable = sshdConfig `File.containsLine` portline
		`describe` ("ssh listening on " ++ portline)
		`onChange` restarted
	disable = sshdConfig `File.lacksLine` portline
		`describe` ("ssh not listening on " ++ portline)
		`onChange` restarted

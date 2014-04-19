module Propellor.Property.Ssh (
	setSshdConfig,
	permitRootLogin,
	passwordAuthentication,
	hasAuthorizedKeys,
	restartSshd,
	randomHostKeys,
	hostKey,
	keyImported,
	knownHost,
	authorizedKeys
) where

import Propellor
import qualified Propellor.Property.File as File
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
	`onChange` restartSshd
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

restartSshd :: Property
restartSshd = cmdProperty "service" ["ssh", "restart"]

-- | Blows away existing host keys and make new ones.
-- Useful for systems installed from an image that might reuse host keys.
-- A flag file is used to only ever do this once.
randomHostKeys :: Property
randomHostKeys = flagFile prop "/etc/ssh/.unique_host_keys"
	`onChange` restartSshd
  where
	prop = property "ssh random host keys" $ do
		void $ liftIO $ boolSystem "sh"
			[ Param "-c"
			, Param "rm -f /etc/ssh/ssh_host_*"
			]
		ensureProperty $
			cmdProperty "/var/lib/dpkg/info/openssh-server.postinst"
				["configure"]

-- | Sets ssh host keys from the site's PrivData.
-- 
-- (Uses a null username for host keys.)
hostKey :: SshKeyType -> Property
hostKey keytype = combineProperties desc
	[ property desc (install writeFile (SshPubKey keytype "") ".pub")
	, property desc (install writeFileProtected (SshPrivKey keytype "") "")
	]
	`onChange` restartSshd
  where
 	desc = "known ssh host key (" ++ fromKeyType keytype ++ ")"
	install writer p ext = withPrivData p $ \key -> do
		let f = "/etc/ssh/ssh_host_" ++ fromKeyType keytype ++ "_key" ++ ext
		s <- liftIO $ readFileStrict f
		if s == key
			then noChange
			else makeChange $ writer f key

-- | Sets up a user with a ssh private key and public key pair
-- from the site's PrivData.
keyImported :: SshKeyType -> UserName -> Property
keyImported keytype user = combineProperties desc
	[ property desc (install writeFile (SshPubKey keytype user) ".pub")
	, property desc (install writeFileProtected (SshPrivKey keytype user) "")
	]
  where
	desc = user ++ " has ssh key (" ++ fromKeyType keytype ++ ")"
	install writer p ext = do
		f <- liftIO $ keyfile ext
		ifM (liftIO $ doesFileExist f)
			( noChange
			, ensureProperty $ combineProperties desc
				[ property desc $ 
					withPrivData p $ \key -> makeChange $
						writer f key
				, File.ownerGroup f user user
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
authorizedKeys :: UserName -> Property
authorizedKeys user = property (user ++ " has authorized_keys") $
	withPrivData (SshAuthorizedKeys user) $ \v -> do
		f <- liftIO $ dotFile "authorized_keys" user
		liftIO $ do
			createDirectoryIfMissing True (takeDirectory f)
			writeFileProtected f v
		ensureProperty $ File.ownerGroup f user user

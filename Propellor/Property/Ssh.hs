module Propellor.Property.Ssh (
	setSshdConfig,
	permitRootLogin,
	passwordAuthentication,
	hasAuthorizedKeys,
	restartSshd,
	uniqueHostKeys
) where

import Propellor
import qualified Propellor.Property.File as File
import Propellor.Property.User
import Utility.SafeCommand

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

hasAuthorizedKeys :: UserName -> IO Bool
hasAuthorizedKeys = go <=< homedir
  where
	go Nothing = return False
	go (Just home) = not . null <$> catchDefaultIO ""
		(readFile $ home </> ".ssh" </> "authorized_keys")

restartSshd :: Property
restartSshd = cmdProperty "service" ["ssh", "restart"]

-- | Blows away existing host keys and make new ones.
-- Useful for systems installed from an image that might reuse host keys.
-- A flag file is used to only ever do this once.
uniqueHostKeys :: Property
uniqueHostKeys = flagFile prop "/etc/ssh/.unique_host_keys"
	`onChange` restartSshd
  where
	prop = Property "ssh unique host keys" $ do
		void $ liftIO $ boolSystem "sh"
			[ Param "-c"
			, Param "rm -f /etc/ssh/ssh_host_*"
			]
		ensureProperty $
			cmdProperty "/var/lib/dpkg/info/openssh-server.postinst"
				["configure"]

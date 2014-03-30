module Property.Ssh where

import Common
import qualified Property.File as File
import Property.User

sshBool :: Bool -> String
sshBool True = "yes"
sshBool False = "no"

sshdConfig :: FilePath
sshdConfig = "/etc/ssh/sshd_config"

setSshdConfig :: String -> Bool -> Property
setSshdConfig setting allowed = combineProperties
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
restartSshd = cmdProperty "service" [Param "ssh", Param "restart"]

{- Blow away existing host keys and make new ones. Use a flag
 - file to prevent doing this more than once. -}
uniqueHostKeys :: Property
uniqueHostKeys = flagFile prop "/etc/ssh/.unique_host_keys"
	`onChange` restartSshd
  where
	prop = Property "ssh unique host keys" $ do
		void $ boolSystem "sh"
			[ Param "-c"
			, Param "rm -f /etc/ssh/ssh_host_*"
			]
		ensureProperty $
			cmdProperty "/var/lib/dpkg/info/openssh-server.postinst"
				[Param "configure"]

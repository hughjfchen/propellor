module Property.Ssh where

import Control.Applicative
import Control.Monad
import System.FilePath

import Property
import Property.User
import Utility.SafeCommand
import Utility.Exception

sshBool :: Bool -> String
sshBool True = "yes"
sshBool False = "no"

sshdConfig :: FilePath
sshdConfig = "/etc/ssh/sshd_config"

setSshdConfig :: String -> Bool -> Property
setSshdConfig setting allowed = combineProperties desc
	[ lineNotInFile sshdConfig (setting ++ sshBool (not allowed))
	, lineInFile sshdConfig (setting ++ sshBool allowed)
	] `onChange` restartSshd
  where
	desc = unwords [ "ssh config:", setting, sshBool allowed ]

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
restartSshd = cmdProperty "service" [Param "sshd", Param "restart"]

{- Blow away existing host keys and make new ones. Use a flag
 - file to prevent doing this more than once. -}
uniqueHostKeys :: Property
uniqueHostKeys = flagFile prop "/etc/ssh/.unique_host_keys"
	`onChange` restartSshd
  where
	prop = IOProperty "ssh unique host keys" $ do
		void $ boolSystem "sh"
			[ Param "-c"
			, Param "rm -f /etc/ssh/ssh_host_*"
			]
		ensureProperty $
			cmdProperty "/var/lib/dpkg/info/openssh-server.postinst"
				[Param "configure"]

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
restartSshd = CmdProperty "ssh restart" "service" [Param "sshd", Param "restart"]

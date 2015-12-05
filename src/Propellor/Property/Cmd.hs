{-# LANGUAGE PackageImports #-}

module Propellor.Property.Cmd (
	-- * Properties for running commands and scripts
	cmdProperty,
	cmdProperty',
	cmdPropertyEnv,
	Script,
	scriptProperty,
	userScriptProperty,
	-- * Lower-level interface for running commands
	CommandParam(..),
	boolSystem,
	boolSystemEnv,
	safeSystem,
	safeSystemEnv,
	shellEscape,
	createProcess,
	waitForProcess,
) where

import Control.Applicative
import Data.List
import "mtl" Control.Monad.Reader

import Propellor.Types
import Propellor.Property
import Utility.SafeCommand
import Utility.Env
import Utility.Process (createProcess, CreateProcess, waitForProcess)

-- | A property that can be satisfied by running a command.
--
-- The command must exit 0 on success.
--
-- This and other properties in this module are `UncheckedProperty`,
-- and return `NoChange`. It's up to the user to check if the command
-- made a change to the system, perhaps by using `checkResult` or
-- `changesFile`, or you can use @cmdProperty "foo" ["bar"] `assume` MadeChange@
cmdProperty :: String -> [String] -> UncheckedProperty NoInfo
cmdProperty cmd params = cmdProperty' cmd params id

cmdProperty' :: String -> [String] -> (CreateProcess -> CreateProcess) -> UncheckedProperty NoInfo
cmdProperty' cmd params mkprocess = unchecked $ property desc $ liftIO $
	cmdResult <$> boolSystem' cmd (map Param params) mkprocess
  where
	desc = unwords $ cmd : params

cmdResult :: Bool -> Result
cmdResult False = FailedChange
cmdResult True = NoChange

-- | A property that can be satisfied by running a command,
-- with added environment variables in addition to the standard
-- environment.
cmdPropertyEnv :: String -> [String] -> [(String, String)] -> UncheckedProperty NoInfo
cmdPropertyEnv cmd params env = unchecked $ property desc $ liftIO $ do
	env' <- addEntries env <$> getEnvironment
	cmdResult <$> boolSystemEnv cmd (map Param params) (Just env')
  where
	desc = unwords $ cmd : params

-- | A series of shell commands. (Without a leading hashbang.)
type Script = [String]

-- | A property that can be satisfied by running a script.
scriptProperty :: Script -> UncheckedProperty NoInfo
scriptProperty script = cmdProperty "sh" ["-c", shellcmd]
  where
	shellcmd = intercalate " ; " ("set -e" : script)

-- | A property that can satisfied by running a script
-- as user (cd'd to their home directory).
userScriptProperty :: User -> Script -> UncheckedProperty NoInfo
userScriptProperty (User user) script = cmdProperty "su" ["--shell", "/bin/sh", "-c", shellcmd, user]
  where
	shellcmd = intercalate " ; " ("set -e" : "cd" : script)

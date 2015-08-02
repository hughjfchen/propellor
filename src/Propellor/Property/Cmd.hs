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
) where

import Control.Applicative
import Data.List
import "mtl" Control.Monad.Reader

import Propellor.Types
import Propellor.Property
import Utility.SafeCommand
import Utility.Env
import Utility.Process (createProcess, CreateProcess)

-- | A property that can be satisfied by running a command.
--
-- The command must exit 0 on success.
cmdProperty :: String -> [String] -> Property NoInfo
cmdProperty cmd params = cmdProperty' cmd params id

cmdProperty' :: String -> [String] -> (CreateProcess -> CreateProcess) -> Property NoInfo
cmdProperty' cmd params mkprocess = property desc $ liftIO $ do
	toResult <$> boolSystem' cmd (map Param params) mkprocess
  where
	desc = unwords $ cmd : params

-- | A property that can be satisfied by running a command,
-- with added environment variables in addition to the standard
-- environment.
cmdPropertyEnv :: String -> [String] -> [(String, String)] -> Property NoInfo
cmdPropertyEnv cmd params env = property desc $ liftIO $ do
	env' <- addEntries env <$> getEnvironment
	toResult <$> boolSystemEnv cmd (map Param params) (Just env')
  where
	desc = unwords $ cmd : params

-- | A series of shell commands. (Without a leading hashbang.)
type Script = [String]

-- | A property that can be satisfied by running a script.
scriptProperty :: Script -> Property NoInfo
scriptProperty script = cmdProperty "sh" ["-c", shellcmd]
  where
	shellcmd = intercalate " ; " ("set -e" : script)

-- | A property that can satisfied by running a script
-- as user (cd'd to their home directory).
userScriptProperty :: User -> Script -> Property NoInfo
userScriptProperty (User user) script = cmdProperty "su" ["--shell", "/bin/sh", "-c", shellcmd, user]
  where
	shellcmd = intercalate " ; " ("set -e" : "cd" : script)

{-# LANGUAGE PackageImports #-}

module Propellor.Property.Cmd (
	cmdProperty,
	cmdProperty',
	cmdPropertyEnv,
	scriptProperty,
	userScriptProperty,
) where

import Control.Applicative
import Data.List
import "mtl" Control.Monad.Reader
import System.Process (CreateProcess)

import Propellor.Types
import Propellor.Property
import Utility.SafeCommand
import Utility.Env

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

-- | A property that can be satisfied by running a series of shell commands.
scriptProperty :: [String] -> Property NoInfo
scriptProperty script = cmdProperty "sh" ["-c", shellcmd]
  where
	shellcmd = intercalate " ; " ("set -e" : script)

-- | A property that can satisfied by running a series of shell commands,
-- as user (cd'd to their home directory).
userScriptProperty :: User -> [String] -> Property NoInfo
userScriptProperty (User user) script = cmdProperty "su" ["--shell", "/bin/sh", "-c", shellcmd, user]
  where
	shellcmd = intercalate " ; " ("set -e" : "cd" : script)

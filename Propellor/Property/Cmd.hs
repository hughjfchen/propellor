{-# LANGUAGE PackageImports #-}

module Propellor.Property.Cmd (
	cmdProperty,
	cmdProperty',
	scriptProperty,
	userScriptProperty,
) where

import Control.Applicative
import Data.List
import "mtl" Control.Monad.Reader

import Propellor.Types
import Propellor.Property
import Utility.Monad
import Utility.SafeCommand
import Utility.Env

-- | A property that can be satisfied by running a command.
--
-- The command must exit 0 on success.
cmdProperty :: String -> [String] -> Property
cmdProperty cmd params = cmdProperty' cmd params []

-- | A property that can be satisfied by running a command,
-- with added environment.
cmdProperty' :: String -> [String] -> [(String, String)] -> Property
cmdProperty' cmd params env = property desc $ liftIO $ do
	env' <- addEntries env <$> getEnvironment
	ifM (boolSystemEnv cmd (map Param params) (Just env'))
		( return MadeChange
		, return FailedChange
		)
  where
  	desc = unwords $ cmd : params

-- | A property that can be satisfied by running a series of shell commands.
scriptProperty :: [String] -> Property
scriptProperty script = cmdProperty "sh" ["-c", shellcmd]
  where
	shellcmd = intercalate " ; " ("set -e" : script)

-- | A property that can satisfied by running a series of shell commands,
-- as user (cd'd to their home directory).
userScriptProperty :: UserName -> [String] -> Property
userScriptProperty user script = cmdProperty "su" ["-c", shellcmd, user]
  where
	shellcmd = intercalate " ; " ("set -e" : "cd" : script)

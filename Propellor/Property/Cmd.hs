module Propellor.Property.Cmd (
	cmdProperty,
	cmdProperty',
	scriptProperty,
	serviceRunning,
) where

import Control.Monad
import Control.Applicative
import Data.List

import Propellor.Types
import Propellor.Engine
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
cmdProperty' cmd params env = Property desc $ do
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

-- | Ensures that a service is running.
--
-- Note that due to the general poor state of init scripts, the best
-- we can do is try to start the service, and if it fails, assume
-- this means it's already running.
serviceRunning :: String -> Property
serviceRunning svc = Property ("running " ++ svc) $ do
	void $ ensureProperty $
		scriptProperty ["service " ++ shellEscape svc ++ " start >/dev/null 2>&1 || true"]
	return NoChange

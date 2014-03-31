module Propellor.Property.Cmd (
	cmdProperty,
	cmdProperty',
	scriptProperty,
	module Utility.SafeCommand
) where

import Control.Applicative
import Data.List

import Propellor.Types
import Utility.Monad
import Utility.SafeCommand
import Utility.Env

cmdProperty :: String -> [CommandParam] -> Property
cmdProperty cmd params = cmdProperty' cmd params []

cmdProperty' :: String -> [CommandParam] -> [(String, String)] -> Property
cmdProperty' cmd params env = Property desc $ do
	env' <- addEntries env <$> getEnvironment
	ifM (boolSystemEnv cmd params (Just env'))
		( return MadeChange
		, return FailedChange
		)
  where
  	desc = unwords $ cmd : map showp params
	showp (Params s) = s
	showp (Param s) = s
	showp (File s) = s

scriptProperty :: [String] -> Property
scriptProperty script = cmdProperty "sh" [Param "-c", Param shellcmd]
  where
	shellcmd = intercalate " ; " ("set -e" : script)

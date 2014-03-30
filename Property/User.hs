module Property.User where

import System.Posix
import Control.Applicative
import Data.Maybe

import Property
import Utility.SafeCommand
import Utility.Exception
import Utility.Process

type UserName = String

nonsystem :: UserName -> Property
nonsystem user = check (isNothing <$> homedir user) $ cmdProperty "adduser"
	[ Param "--disabled-password"
	, Param "--gecos", Param ""
	, Param user
	]

{- Removes user home directory!! Use with caution. -}
nuked :: UserName -> Property
nuked user = check (isJust <$> homedir user) $ cmdProperty "userdel"
	[ Param "-r"
	, Param user
	]

lockedPassword :: UserName -> Property
lockedPassword user = check (not <$> isLockedPassword user) $ cmdProperty "passwd"
	[ Param "--lock"
	, Param user
	]

isLockedPassword :: UserName -> IO Bool
isLockedPassword user = parse . words <$> readProcess "passwd" ["-S", user]
  where
	parse (_:"L":_) = True
	parse _ = False

homedir :: UserName -> IO (Maybe FilePath)
homedir user = catchMaybeIO $ homeDirectory <$> getUserEntryForName user

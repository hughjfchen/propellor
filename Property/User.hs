module Property.User where

import System.Posix
import Control.Applicative
import Data.Maybe

import Property
import Utility.SafeCommand
import Utility.Exception

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
lockedPassword user = cmdProperty "passwd"
	[ Param "--lock"
	, Param user
	]

homedir :: UserName -> IO (Maybe FilePath)
homedir user = catchMaybeIO $ homeDirectory <$> getUserEntryForName user

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

homedir :: UserName -> IO (Maybe FilePath)
homedir user = catchMaybeIO $ homeDirectory <$> getUserEntryForName user

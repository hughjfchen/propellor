module Property.User where

import System.Posix

import Common

type UserName = String

sshAccountFor :: UserName -> Property
sshAccountFor user = check (isNothing <$> homedir user) $ cmdProperty "adduser"
	[ Param "--disabled-password"
	, Param "--gecos", Param ""
	, Param user
	]
	`describe` ("ssh account " ++ user)

{- Removes user home directory!! Use with caution. -}
nuked :: UserName -> Property
nuked user = check (isJust <$> homedir user) $ cmdProperty "userdel"
	[ Param "-r"
	, Param user
	]
	`describe` ("nuked user " ++ user)

lockedPassword :: UserName -> Property
lockedPassword user = check (not <$> isLockedPassword user) $ cmdProperty "passwd"
	[ Param "--lock"
	, Param user
	]
	`describe` ("locked " ++ user ++ " password")

isLockedPassword :: UserName -> IO Bool
isLockedPassword user = parse . words <$> readProcess "passwd" ["-S", user]
  where
	parse (_:"L":_) = True
	parse _ = False

homedir :: UserName -> IO (Maybe FilePath)
homedir user = catchMaybeIO $ homeDirectory <$> getUserEntryForName user

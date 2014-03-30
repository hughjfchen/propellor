module Property.User where

import System.Posix

import Common

data Eep = YesReallyDeleteHome

sshAccountFor :: UserName -> Property
sshAccountFor user = check (isNothing <$> homedir user) $ cmdProperty "adduser"
	[ Param "--disabled-password"
	, Param "--gecos", Param ""
	, Param user
	]
	`describe` ("ssh account " ++ user)

{- Removes user home directory!! Use with caution. -}
nuked :: UserName -> Eep -> Property
nuked user _ = check (isJust <$> homedir user) $ cmdProperty "userdel"
	[ Param "-r"
	, Param user
	]
	`describe` ("nuked user " ++ user)

setPassword :: UserName -> Property
setPassword user = Property (user ++ " password set") $
	withPrivData (Password user) $ \password -> makeChange $
		withHandle StdinHandle createProcessSuccess
			(proc "chpasswd" []) $ \h -> do
				hPutStrLn h $ user ++ ":" ++ password
				hClose h

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

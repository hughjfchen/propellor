module Propellor.Property.User where

import System.Posix

import Propellor

data Eep = YesReallyDeleteHome

accountFor :: UserName -> Property
accountFor user = check (isNothing <$> catchMaybeIO (homedir user)) $ cmdProperty "adduser"
	[ "--disabled-password"
	, "--gecos", ""
	, user
	]
	`describe` ("account for " ++ user)

-- | Removes user home directory!! Use with caution.
nuked :: UserName -> Eep -> Property
nuked user _ = check (isJust <$> catchMaybeIO (homedir user)) $ cmdProperty "userdel"
	[ "-r"
	, user
	]
	`describe` ("nuked user " ++ user)

-- | Only ensures that the user has some password set. It may or may
-- not be the password from the PrivData.
hasSomePassword :: UserName -> Property
hasSomePassword user = hasSomePassword' user hostContext

-- | While hasSomePassword uses the name of the host as context,
-- this allows specifying a different context. This is useful when
-- you want to use the same password on multiple hosts, for example.
hasSomePassword' :: IsContext c => UserName -> c -> Property
hasSomePassword' user context = check ((/= HasPassword) <$> getPasswordStatus user) $
	hasPassword' user context

-- | Ensures that a user's password is set to the password from the PrivData.
-- (Will change any existing password.)
hasPassword :: UserName -> Property
hasPassword user = hasPassword' user hostContext

hasPassword' :: IsContext c => UserName -> c -> Property
hasPassword' user context = go `requires` shadowConfig True
  where
	go = withPrivData (Password user) context $
		property (user ++ " has password") . setPassword user

setPassword :: UserName -> ((PrivData -> Propellor Result) -> Propellor Result) -> Propellor Result
setPassword user getpassword = getpassword $ \password -> makeChange $
	withHandle StdinHandle createProcessSuccess
		(proc "chpasswd" []) $ \h -> do
			hPutStrLn h $ user ++ ":" ++ password
			hClose h

lockedPassword :: UserName -> Property
lockedPassword user = check (not <$> isLockedPassword user) $ cmdProperty "passwd"
	[ "--lock"
	, user
	]
	`describe` ("locked " ++ user ++ " password")

data PasswordStatus = NoPassword | LockedPassword | HasPassword
	deriving (Eq)

getPasswordStatus :: UserName -> IO PasswordStatus
getPasswordStatus user = parse . words <$> readProcess "passwd" ["-S", user]
  where
	parse (_:"L":_) = LockedPassword
	parse (_:"NP":_) = NoPassword
	parse (_:"P":_) = HasPassword
	parse _ = NoPassword

isLockedPassword :: UserName -> IO Bool
isLockedPassword user = (== LockedPassword) <$> getPasswordStatus user

homedir :: UserName -> IO FilePath
homedir user = homeDirectory <$> getUserEntryForName user

hasGroup :: UserName -> GroupName -> Property
hasGroup user group' = check test $ cmdProperty "adduser"
	[ user
	, group'
	]
	`describe` unwords ["user", user, "in group", group']
  where
	test = not . elem group' . words <$> readProcess "groups" [user]

-- | Controls whether shadow passwords are enabled or not.
shadowConfig :: Bool -> Property
shadowConfig True = check (not <$> shadowExists) $
	cmdProperty "shadowconfig" ["on"]
		`describe` "shadow passwords enabled"
shadowConfig False = check shadowExists $
	cmdProperty "shadowconfig" ["off"]
		`describe` "shadow passwords disabled"

shadowExists :: IO Bool
shadowExists = doesFileExist "/etc/shadow"

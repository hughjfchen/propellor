module Propellor.Property.Sudo where

import Data.List

import Propellor
import Propellor.Property.File
import qualified Propellor.Property.Apt as Apt
import Propellor.Property.User

-- | Allows a user to sudo. If the user has a password, sudo is configured
-- to require it. If not, NOPASSWORD is enabled for the user.
enabledFor :: UserName -> Property
enabledFor user = property desc go `requires` Apt.installed ["sudo"]
  where
	go = do
		locked <- liftIO $ isLockedPassword user
		ensureProperty $
			fileProperty desc
				(modify locked . filter (wanted locked))
				"/etc/sudoers"
	desc = user ++ " is sudoer"
	sudobaseline = user ++ " ALL=(ALL:ALL)"
	sudoline True = sudobaseline ++ " NOPASSWD:ALL"
	sudoline False = sudobaseline ++ " ALL"
	wanted locked l
		-- TOOD: Full sudoers file format parse.. 
		| not (sudobaseline `isPrefixOf` l) = True
		| "NOPASSWD" `isInfixOf` l = locked
		| otherwise = True
 	modify locked ls
		| sudoline locked `elem` ls = ls
		| otherwise = ls ++ [sudoline locked]

module Propellor.Property.Sudo where

import Data.List

import Propellor.Base
import Propellor.Property.File
import qualified Propellor.Property.Apt as Apt
import Propellor.Property.User

-- | Allows a user to sudo. If the user has a password, sudo is configured
-- to require it. If not, NOPASSWORD is enabled for the user.
enabledFor :: User -> RevertableProperty DebianLike DebianLike
enabledFor user@(User u) = setup `requires` Apt.installed ["sudo"] <!> cleanup
  where
	setup :: Property UnixLike
	setup = property' desc $ \w -> do
		locked <- liftIO $ isLockedPassword user
		ensureProperty w $
			fileProperty desc
				(modify locked . filter (wanted locked))
				sudoers
	  where
		desc = u ++ " is sudoer"
	
	cleanup :: Property DebianLike
	cleanup = tightenTargets $ 
		fileProperty desc (filter notuserline) sudoers
	  where
		desc = u ++ " is not sudoer"
	
	sudoers = "/etc/sudoers"
	sudobaseline = u ++ " ALL=(ALL:ALL)"
	notuserline l = not (sudobaseline `isPrefixOf` l)
	sudoline True = sudobaseline ++ " NOPASSWD:ALL"
	sudoline False = sudobaseline ++ " ALL"
	wanted locked l
		-- TOOD: Full sudoers file format parse.. 
		| notuserline l = True
		| "NOPASSWD" `isInfixOf` l = locked
		| otherwise = True
	modify locked ls
		| sudoline locked `elem` ls = ls
		| otherwise = ls ++ [sudoline locked]

{-# LANGUAGE FlexibleInstances #-}

module Propellor.Property.LightDM where

import Propellor
import qualified Propellor.Property.File as File

-- | Configures LightDM to skip the login screen and autologin as a user.
autoLogin :: User -> Property NoInfo
autoLogin (User u) = "/etc/lightdm/lightdm.conf" `File.containsConfPair`
	                 ("SeatDefaults", "autologin-user", u)
	                 `describe` "lightdm autologin"

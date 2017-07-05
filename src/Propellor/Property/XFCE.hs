module Propellor.Property.XFCE where

import Propellor.Base
import Propellor.Types.Core (getSatisfy)
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.File as File
import qualified Propellor.Property.User as User

installed :: Property DebianLike
installed = Apt.installed ["task-xfce-desktop"]
	`describe` "XFCE desktop installed"

-- | Minimal install of XFCE, with a terminal emulator and panel,
-- and X, but not any of the extras.
installedMin :: Property DebianLike
installedMin = Apt.installedMin ["xfce4", "xfce4-terminal", "task-desktop"]
	`describe` "minimal XFCE desktop installed"

-- | Normally at first login, XFCE asks what kind of panel the user wants.
-- This enables the default configuration noninteractively.
--
-- If the user subsequently modifies their panel, their modifications will
-- not be overwritten by this property.
defaultPanelFor :: User -> Property DebianLike
defaultPanelFor u@(User username) = adjustPropertySatisfy baseprop $ \s -> do
	home <- liftIO $ User.homedir u
	s <> fromMaybe mempty (getSatisfy (go home))
  where
	cf = ".config" </> "xfce4" </> "xfconf"
		</> "xfce-perchannel-xml" </> "xfce4-panel.xml"
	-- This location is probably Debian-specific.
	defcf = "/etc/xdg/xfce4/panel/default.xml"
	go :: FilePath -> Property DebianLike
	go home = tightenTargets $
		(home </> cf) `File.isCopyOf` defcf
			`before` File.applyPath home cf
				(\f -> File.ownerGroup f u (userGroup u))
			`requires` Apt.installed ["xfce4-panel"]
	baseprop :: Property DebianLike
	baseprop = doNothing `describe` ("default XFCE panel for " ++ username)

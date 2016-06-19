-- | Maintainer: Sean Whitton <spwhitton@spwhitton.name>

{-# LANGUAGE DeriveDataTypeable #-}

module Propellor.Property.Schroot where

import Propellor.Base
import Propellor.Types.Info
import qualified Propellor.Property.File as File
import qualified Propellor.Property.Apt as Apt

import Utility.FileMode

data UseOverlays = UseOverlays deriving (Eq, Show, Typeable)

-- | Indicate that a schroots on a host should use @union-type=overlay@
--
-- Setting this property does not actually ensure that the line
-- @union-type=overlay@ is present in any schroot config files.  See
-- 'Propellor.Property.Sbuild.built' for example usage.
--
-- You should apply this property to a host before any properties that can use
-- overlays.  For example, use
--
-- >  & Schroot.useOverlays
-- >  & Sbuild.builtFor (System (Debian Unstable) X86_32)
--
-- rather than
--
-- >  & Sbuild.builtFor (System (Debian Unstable) X86_32)
-- >  & Schroot.useOverlays
useOverlays :: Property (HasInfo + UnixLike)
useOverlays = pureInfoProperty "use schroot overlays" (InfoVal UseOverlays)

-- | Gets whether a host uses overlays.
usesOverlays :: Propellor Bool
usesOverlays = isJust . fromInfoVal
	<$> (askInfo :: Propellor (InfoVal UseOverlays))

-- | Configure schroot such that all schroots with @union-type=overlay@ in their
-- configuration will run their overlays in a tmpfs.
--
-- Implicitly sets 'useOverlays' info property.  Like that property, you should
-- apply 'overlaysInTmpfs' to a host before applying any properties that can use
-- overlays (e.g. 'Propellor.Property.Sbuild.built').
--
-- Shell script from <https://wiki.debian.org/sbuild>.
overlaysInTmpfs :: Property (HasInfo + DebianLike)
overlaysInTmpfs = go `requires` installed
  where
	f = "/etc/schroot/setup.d/04tmpfs"
	go :: Property (HasInfo + UnixLike)
	go = combineProperties "schroot overlays in tmpfs" $ props
		& useOverlays
		& f `File.hasContent`
			[ "#!/bin/sh"
			, ""
			, "set -e"
			, ""
			, ". \"$SETUP_DATA_DIR/common-data\""
			, ". \"$SETUP_DATA_DIR/common-functions\""
			, ". \"$SETUP_DATA_DIR/common-config\""
			, ""
			, ""
			, "if [ $STAGE = \"setup-start\" ]; then"
			, "  mount -t tmpfs overlay /var/lib/schroot/union/overlay"
			, "elif [ $STAGE = \"setup-recover\" ]; then"
			, "  mount -t tmpfs overlay /var/lib/schroot/union/overlay"
			, "elif [ $STAGE = \"setup-stop\" ]; then"
			, "  umount -f /var/lib/schroot/union/overlay"
			, "fi"
			]
		`onChange` (f `File.mode` combineModes (readModes ++ executeModes))

installed :: Property DebianLike
installed = Apt.installed ["schroot"]

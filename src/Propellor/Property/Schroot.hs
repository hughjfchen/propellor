-- | Maintainer: Sean Whitton <spwhitton@spwhitton.name>

module Propellor.Property.Schroot where

import Propellor.Base
import qualified Propellor.Property.File as File
import qualified Propellor.Property.Apt as Apt

import Utility.FileMode

-- | Configure schroot such that all schroots with @union-type=overlay@ in their
-- configuration will run their overlays in a tmpfs.
--
-- Shell script from <https://wiki.debian.org/sbuild>.
overlaysInTmpfs :: Property DebianLike
overlaysInTmpfs = go `requires` installed
  where
	f = "/etc/schroot/setup.d/04tmpfs"
	go :: Property UnixLike
	go = f `File.hasContent`
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
		`onChange` (f `File.mode` (combineModes (readModes ++ executeModes)))
		`describe` "schroot overlays in tmpfs"

installed :: Property DebianLike
installed = Apt.installed ["schroot"]

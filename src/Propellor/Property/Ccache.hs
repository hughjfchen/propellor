-- | Maintainer: Sean Whitton <spwhitton@spwhitton.name>

module Propellor.Property.Ccache where

import Propellor.Base
import qualified Propellor.Property.File as File
import qualified Propellor.Property.Apt as Apt

import Utility.FileMode
import System.Posix.Files

-- | Configures a ccache in /var/cache for a group
--
-- If you say
--
--  >  & (Group "foo") `Ccache.hasGroupCache` "4G"
--
-- you instruct propellor to create a ccache in /var/cache/ccache-foo owned and
-- writeable by the foo group, with a maximum cache size of 4GB.  See ccache(1)
-- for size specification.
hasGroupCache :: Group -> String -> RevertableProperty DebianLike UnixLike
group@(Group g) `hasGroupCache` size = (make `requires` installed) <!> delete
  where
	path = "/var/cache/ccache-" ++ g
	make = check (not <$> doesDirectoryExist path) $
		propertyList ("ccache for " ++ g ++ " exists") $ props
			& File.dirExists path
			& File.ownerGroup path (User "root") group
			& File.mode path (combineModes $
				readModes ++ executeModes
 				++ [ownerWriteMode, groupWriteMode])
			& cmdProperty "ccache" ["--max-size", size]
			`assume` MadeChange
	delete = check (doesDirectoryExist path) $
		cmdProperty "rm" ["-r", path] `assume` MadeChange
		`describe` ("ccache for " ++ g ++ " does not exist")

installed :: Property DebianLike
installed = Apt.installed ["ccache"]

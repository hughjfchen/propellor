-- | Maintainer: Sean Whitton <spwhitton@spwhitton.name>

module Propellor.Property.Ccache where

import Propellor.Base
import qualified Propellor.Property.File as File
import qualified Propellor.Property.Apt as Apt

import Utility.FileMode
import System.Posix.Files

-- | Limits on the size of a ccache
data Limit
	-- | The maximum size of the cache, as a string such as "4G"
	--
	-- See ccache(1) for more on the size specification.
	= MaxSize String
	-- | The maximum number of files in the cache
	| MaxFiles Int
	-- | A cache with no limit specified
	| NoLimit

-- | Configures a ccache in /var/cache for a group
--
-- If you say
--
--  >  & (Group "foo") `Ccache.hasGroupCache` (Ccache.MaxSize "4g")
--
-- you instruct propellor to create a ccache in /var/cache/ccache-foo owned and
-- writeable by the foo group, with a maximum cache size of 4GB.
--
-- It is safe to specify this property more than once for a given group if you
-- wish to limit both the maximum size of the cache and the maximum number of
-- files in the cache.  However, setting only one of these two limits is
-- generally sufficient.
hasGroupCache :: Group -> Limit -> RevertableProperty DebianLike UnixLike
group@(Group g) `hasGroupCache` limit = (make `requires` installed) <!> delete
  where
	make = propertyList ("ccache for " ++ g ++ " group exists") $ props
			& File.dirExists path
			& File.ownerGroup path (User "root") group
			& File.mode path (combineModes $
				readModes ++ executeModes
				++ [ownerWriteMode, groupWriteMode])
			& case limit of
				NoLimit    -> doNothing
				MaxSize  s -> setSizeLimit s
				MaxFiles f -> setFileLimit (show f)

	delete = check (doesDirectoryExist path) $
		cmdProperty "rm" ["-r", path] `assume` MadeChange
		`describe` ("ccache for " ++ g ++ " does not exist")

	setSizeLimit s = conf `File.containsLine` ("max_size = " ++ s)
	setFileLimit f = conf `File.containsLine` ("max_files = " ++ f)
	path = "/var/cache/ccache-" ++ g
	conf = path </> "ccache.conf"

installed :: Property DebianLike
installed = Apt.installed ["ccache"]

-- | Maintainer: Félix Sipma <felix+propellor@gueux.org>

module Propellor.Property.DebianMirror
	( DebianPriority (..)
	, showPriority
	, mirror
	, RsyncExtra (..)
	, Method (..)
	, DebianMirror
	, setDebianMirrorHostName
	, setDebianMirrorSuites
	, setDebianMirrorArchitectures
	, setDebianMirrorSections
	, setDebianMirrorSourceBool
	, setDebianMirrorPriorities
	, setDebianMirrorMethod
	, setDebianMirrorKeyring
	, setDebianMirrorRsyncExtra
	, mkDebianMirror
	) where

import Propellor.Base
import qualified Propellor.Property.File as File
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.Cron as Cron
import qualified Propellor.Property.User as User

import Data.List


data DebianPriority = Essential | Required | Important | Standard | Optional | Extra
	deriving (Show, Eq)

showPriority :: DebianPriority -> String
showPriority Essential = "essential"
showPriority Required  = "required"
showPriority Important = "important"
showPriority Standard  = "standard"
showPriority Optional  = "optional"
showPriority Extra     = "extra"

data RsyncExtra = Doc | Indices | Tools | Trace
	deriving (Show, Eq)

showRsyncExtra :: RsyncExtra -> String
showRsyncExtra Doc = "doc"
showRsyncExtra Indices = "indices"
showRsyncExtra Tools = "tools"
showRsyncExtra Trace = "trace"

data Method = Ftp | Http | Https | Rsync | MirrorFile

showMethod :: Method -> String
showMethod Ftp = "ftp"
showMethod Http = "http"
showMethod Https = "https"
showMethod Rsync = "rsync"
showMethod MirrorFile = "file"

data DebianMirror = DebianMirror
	{ debianMirrorHostName :: HostName
	, debianMirrorDir :: FilePath
	, debianMirrorSuites :: [DebianSuite]
	, debianMirrorArchitectures :: [Architecture]
	, debianMirrorSections :: [Apt.Section]
	, debianMirrorSourceBool :: Bool
	, debianMirrorPriorities :: [DebianPriority]
	, debianMirrorMethod :: Method
	, debianMirrorKeyring :: FilePath
	, debianMirrorRsyncExtra :: [RsyncExtra]
	, debianMirrorCronTimes :: Cron.Times
	}

mkDebianMirror :: FilePath -> Cron.Times -> DebianMirror
mkDebianMirror dir crontimes = DebianMirror
	{ debianMirrorHostName = "httpredir.debian.org"
	, debianMirrorDir = dir
	, debianMirrorSuites = []
	, debianMirrorArchitectures = []
	, debianMirrorSections = []
	, debianMirrorSourceBool = False
	, debianMirrorPriorities = []
	, debianMirrorMethod = Http
	, debianMirrorKeyring = "/usr/share/keyrings/debian-archive-keyring.gpg"
	, debianMirrorRsyncExtra = [Trace]
	, debianMirrorCronTimes = crontimes
	}

setDebianMirrorHostName :: HostName -> DebianMirror -> DebianMirror
setDebianMirrorHostName hn m = m { debianMirrorHostName = hn }

setDebianMirrorSuites :: [DebianSuite] -> DebianMirror -> DebianMirror
setDebianMirrorSuites s m = m { debianMirrorSuites = s }

setDebianMirrorArchitectures :: [Architecture] -> DebianMirror -> DebianMirror
setDebianMirrorArchitectures a m = m { debianMirrorArchitectures = a }

setDebianMirrorSections :: [Apt.Section] -> DebianMirror -> DebianMirror
setDebianMirrorSections s m = m { debianMirrorSections = s }

setDebianMirrorSourceBool :: Bool -> DebianMirror -> DebianMirror
setDebianMirrorSourceBool s m = m { debianMirrorSourceBool = s }

setDebianMirrorPriorities :: [DebianPriority] -> DebianMirror -> DebianMirror
setDebianMirrorPriorities p m = m { debianMirrorPriorities = p }

setDebianMirrorMethod :: Method -> DebianMirror -> DebianMirror
setDebianMirrorMethod me m = m { debianMirrorMethod = me }

setDebianMirrorKeyring :: FilePath -> DebianMirror -> DebianMirror
setDebianMirrorKeyring k m = m { debianMirrorKeyring = k }

setDebianMirrorRsyncExtra :: [RsyncExtra] -> DebianMirror -> DebianMirror
setDebianMirrorRsyncExtra r m = m { debianMirrorRsyncExtra = r }

mirror :: DebianMirror -> Property NoInfo
mirror mirror' = propertyList
	("Debian mirror " ++ dir)
	[ Apt.installed ["debmirror"]
	, User.accountFor (User "debmirror")
	, File.dirExists dir
	, File.ownerGroup dir (User "debmirror") (Group "debmirror")
	, check (not . and <$> mapM suitemirrored suites) $ cmdProperty "debmirror" args
		`describe` "debmirror setup"
	, Cron.niceJob ("debmirror_" ++ dir) (debianMirrorCronTimes mirror') (User "debmirror") "/" $
		unwords ("/usr/bin/debmirror" : args)
	]
  where
	dir = debianMirrorDir mirror'
	suites = debianMirrorSuites mirror'
	suitemirrored suite = doesDirectoryExist $ dir </> "dists" </> Apt.showSuite suite
	architecturearg = intercalate ","
	suitearg = intercalate "," $ map Apt.showSuite suites
	priorityRegex pp = "(" ++ intercalate "|" (map showPriority pp) ++ ")"
	rsyncextraarg [] = "none"
	rsyncextraarg res = intercalate "," $ map showRsyncExtra res
	args =
		[ "--dist" , suitearg
		, "--arch", architecturearg (debianMirrorArchitectures mirror')
		, "--section", intercalate "," (debianMirrorSections mirror')
		, "--limit-priority", "\"" ++ priorityRegex (debianMirrorPriorities mirror') ++ "\""
		]
		++
		(if (debianMirrorSourceBool mirror') then [] else ["--nosource"])
		++
		[ "--host", debianMirrorHostName mirror'
		, "--method", showMethod $ debianMirrorMethod mirror'
		, "--rsync-extra", rsyncextraarg $ debianMirrorRsyncExtra mirror'
		, "--keyring", debianMirrorKeyring mirror'
		, dir
		]

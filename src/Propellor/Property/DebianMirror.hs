-- | Maintainer: Félix Sipma <felix+propellor@gueux.org>

module Propellor.Property.DebianMirror
	( DebianPriority(..)
	, showPriority
	, mirror
	, mirrorCdn
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

mirror :: HostName -> FilePath -> [DebianSuite] -> [Architecture] -> [Apt.Section] -> Bool -> [DebianPriority] -> [RsyncExtra] -> Cron.Times -> Property NoInfo
mirror hn dir suites archs sections source priorities rsyncextras crontimes = propertyList
	("Debian mirror " ++ dir)
	[ Apt.installed ["debmirror"]
	, User.accountFor (User "debmirror")
	, File.dirExists dir
	, File.ownerGroup dir (User "debmirror") (Group "debmirror")
	, check (not . and <$> mapM suitemirrored suites) $ cmdProperty "debmirror" args
		`describe` "debmirror setup"
	, Cron.niceJob ("debmirror_" ++ dir) crontimes (User "debmirror") "/" $
		unwords ("/usr/bin/debmirror" : args)
	]
  where
	suitemirrored suite = doesDirectoryExist $ dir </> "dists" </> Apt.showSuite suite
	architecturearg = intercalate ","
	suitearg = intercalate "," $ map Apt.showSuite suites
	priorityRegex pp = "(" ++ intercalate "|" (map showPriority pp) ++ ")"
	rsyncextraarg [] = "none"
	rsyncextraarg res = intercalate "," $ map showRsyncExtra res
	args =
		[ "--dist" , suitearg
		, "--arch", architecturearg archs
		, "--section", intercalate "," sections
		, "--limit-priority", "\"" ++ priorityRegex priorities ++ "\""
		]
		++
		(if source then [] else ["--nosource"])
		++
		[ "--host", hn
		, "--method", "http"
		, "--rsync-extra", rsyncextraarg rsyncextras
		, "--keyring", "/usr/share/keyrings/debian-archive-keyring.gpg"
		, dir
		]

mirrorCdn :: FilePath -> [DebianSuite] -> [Architecture] -> [Apt.Section] -> Bool -> [DebianPriority] -> [RsyncExtra] -> Cron.Times -> Property NoInfo
mirrorCdn = mirror "httpredir.debian.org"

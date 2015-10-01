-- | Maintainer: Félix Sipma <felix+propellor@gueux.org>

module Propellor.Property.DebianMirror
	( DebianPriority(..)
	, showPriority
	, mirror
	, mirrorCdn
	) where

import Propellor
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

mirror :: Apt.Url -> FilePath -> [DebianSuite] -> [Architecture] -> [Apt.Section] -> Bool -> [DebianPriority] -> Cron.Times -> Property NoInfo
mirror url dir suites archs sections source priorities crontimes = propertyList
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
	args =
		[ "--dist" , suitearg
		, "--arch", architecturearg archs
		, "--section", intercalate "," sections
		, "--limit-priority", "\"" ++ priorityRegex priorities ++ "\""
		]
		++
		(if source then [] else ["--nosource"])
		++
		[ "--host", url
		, "--method", "http"
		, "--keyring", "/usr/share/keyrings/debian-archive-keyring.gpg"
		, dir
		]

mirrorCdn :: FilePath -> [DebianSuite] -> [Architecture] -> [Apt.Section] -> Bool -> [DebianPriority] -> Cron.Times -> Property NoInfo
mirrorCdn = mirror "http://httpredir.debian.org/debian"

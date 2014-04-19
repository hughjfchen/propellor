module Propellor.Property.Apt where

import Data.Maybe
import Control.Applicative
import Data.List
import System.IO
import Control.Monad

import Propellor
import qualified Propellor.Property.File as File
import qualified Propellor.Property.Service as Service
import Propellor.Property.File (Line)

sourcesList :: FilePath
sourcesList = "/etc/apt/sources.list"

type Url = String
type Section = String

type SourcesGenerator = DebianSuite -> [Line]

showSuite :: DebianSuite -> String
showSuite Stable = "stable"
showSuite Testing = "testing"
showSuite Unstable = "unstable"
showSuite Experimental = "experimental"
showSuite (DebianRelease r) = r

backportSuite :: String
backportSuite = showSuite stableRelease ++ "-backports"

debLine :: String -> Url -> [Section] -> Line
debLine suite mirror sections = unwords $
	["deb", mirror, suite] ++ sections

srcLine :: Line -> Line
srcLine l = case words l of
	("deb":rest) -> unwords $ "deb-src" : rest
	_ -> ""

stdSections :: [Section]
stdSections = ["main", "contrib", "non-free"]

binandsrc :: String -> SourcesGenerator
binandsrc url suite
	| isStable suite = [l, srcLine l, bl, srcLine bl]
	| otherwise = [l, srcLine l]
  where
	l = debLine (showSuite suite) url stdSections
	bl = debLine backportSuite url stdSections

debCdn :: SourcesGenerator
debCdn = binandsrc "http://cdn.debian.net/debian"

kernelOrg :: SourcesGenerator
kernelOrg = binandsrc "http://mirrors.kernel.org/debian"

-- | Only available for Stable and Testing
securityUpdates :: SourcesGenerator
securityUpdates suite
	| isStable suite || suite == Testing =
		let l = "deb http://security.debian.org/ " ++ showSuite suite ++ "/updates " ++ unwords stdSections
		in [l, srcLine l]
	| otherwise = []

-- | Makes sources.list have a standard content using the mirror CDN,
-- with a particular DebianSuite.
--
-- Since the CDN is sometimes unreliable, also adds backup lines using
-- kernel.org.
stdSourcesList :: DebianSuite -> Property
stdSourcesList suite = stdSourcesList' suite []

-- | Adds additional sources.list generators.
--
-- Note that if a Property needs to enable an apt source, it's better
-- to do so via a separate file in /etc/apt/sources.list.d/
stdSourcesList' :: DebianSuite -> [SourcesGenerator] -> Property
stdSourcesList' suite more = setSourcesList
	(concatMap (\gen -> gen suite) generators)
	`describe` ("standard sources.list for " ++ show suite)
  where
	generators = [debCdn, kernelOrg, securityUpdates] ++ more

setSourcesList :: [Line] -> Property
setSourcesList ls = sourcesList `File.hasContent` ls `onChange` update

setSourcesListD :: [Line] -> FilePath -> Property
setSourcesListD ls basename = f `File.hasContent` ls `onChange` update
  where
	f = "/etc/apt/sources.list.d/" ++ basename ++ ".list"

runApt :: [String] -> Property
runApt ps = cmdProperty' "apt-get" ps noninteractiveEnv

noninteractiveEnv :: [(String, String)]
noninteractiveEnv =
		[ ("DEBIAN_FRONTEND", "noninteractive")
		, ("APT_LISTCHANGES_FRONTEND", "none")
		]

update :: Property
update = runApt ["update"]
	`describe` "apt update"

upgrade :: Property
upgrade = runApt ["-y", "dist-upgrade"]
	`describe` "apt dist-upgrade"

type Package = String

installed :: [Package] -> Property
installed = installed' ["-y"]

installed' :: [String] -> [Package] -> Property
installed' params ps = robustly $ check (isInstallable ps) go
	`describe` (unwords $ "apt installed":ps)
  where
	go = runApt $ params ++ ["install"] ++ ps

installedBackport :: [Package] -> Property
installedBackport ps = trivial $ withOS desc $ \o -> case o of
	Nothing -> error "cannot install backports; os not declared"
	(Just (System (Debian suite) _))
		| isStable suite -> 
			ensureProperty $ runApt $ 
				["install", "-t", backportSuite, "-y"] ++ ps
	_ -> error $ "backports not supported on " ++ show o
  where
	desc = (unwords $ "apt installed backport":ps)

-- | Minimal install of package, without recommends.
installedMin :: [Package] -> Property
installedMin = installed' ["--no-install-recommends", "-y"]

removed :: [Package] -> Property
removed ps = check (or <$> isInstalled' ps) go
	`describe` (unwords $ "apt removed":ps)
  where
	go = runApt $ ["-y", "remove"] ++ ps

buildDep :: [Package] -> Property
buildDep ps = robustly go
	`describe` (unwords $ "apt build-dep":ps)
  where
	go = runApt $ ["-y", "build-dep"] ++ ps

-- | Installs the build deps for the source package unpacked
-- in the specifed directory, with a dummy package also
-- installed so that autoRemove won't remove them.
buildDepIn :: FilePath -> Property
buildDepIn dir = go `requires` installedMin ["devscripts", "equivs"]
  where
	go = cmdProperty' "sh" ["-c", "cd '" ++ dir ++ "' && mk-build-deps debian/control --install --tool 'apt-get -y --no-install-recommends' --remove"]
			noninteractiveEnv

-- | Package installation may fail becuse the archive has changed.
-- Run an update in that case and retry.
robustly :: Property -> Property
robustly p = adjustProperty p $ \satisfy -> do
	r <- satisfy
	if r == FailedChange
		then ensureProperty $ p `requires` update
		else return r

isInstallable :: [Package] -> IO Bool
isInstallable ps = do
	l <- isInstalled' ps
	return $ any (== False) l && not (null l)

isInstalled :: Package -> IO Bool
isInstalled p = (== [True]) <$> isInstalled' [p]

-- | Note that the order of the returned list will not always
-- correspond to the order of the input list. The number of items may
-- even vary. If apt does not know about a package at all, it will not
-- be included in the result list.
isInstalled' :: [Package] -> IO [Bool]
isInstalled' ps = catMaybes . map parse . lines
	<$> readProcess "apt-cache" ("policy":ps)
  where
	parse l
		| "Installed: (none)" `isInfixOf` l = Just False
		| "Installed: " `isInfixOf` l = Just True
		| otherwise = Nothing

autoRemove :: Property
autoRemove = runApt ["-y", "autoremove"]
	`describe` "apt autoremove"

-- | Enables unattended upgrades. Revert to disable.
unattendedUpgrades :: RevertableProperty
unattendedUpgrades = RevertableProperty enable disable
  where
	enable = setup True
		`before` Service.running "cron"
		`before` configure
	disable = setup False

	setup enabled = (if enabled then installed else removed) ["unattended-upgrades"]
		`onChange` reConfigure "unattended-upgrades"
			[("unattended-upgrades/enable_auto_updates" , "boolean", v)]
		`describe` ("unattended upgrades " ++ v)
	  where
		v
			| enabled = "true"
			| otherwise = "false"
	
	configure = withOS "unattended upgrades configured" $ \o ->
		case o of
			-- the package defaults to only upgrading stable
			(Just (System (Debian suite) _))
				| not (isStable suite) -> ensureProperty $
					"/etc/apt/apt.conf.d/50unattended-upgrades"
						`File.containsLine`
					("\t\"o=Debian,a="++showSuite suite++"\";")
			_ -> noChange

-- | Preseeds debconf values and reconfigures the package so it takes
-- effect.
reConfigure :: Package -> [(String, String, String)] -> Property
reConfigure package vals = reconfigure `requires` setselections
	`describe` ("reconfigure " ++ package)
  where
	setselections = property "preseed" $ makeChange $
		withHandle StdinHandle createProcessSuccess
			(proc "debconf-set-selections" []) $ \h -> do
				forM_ vals $ \(tmpl, tmpltype, value) ->
					hPutStrLn h $ unwords [package, tmpl, tmpltype, value]
				hClose h
	reconfigure = cmdProperty' "dpkg-reconfigure" ["-fnone", package] noninteractiveEnv

-- | Ensures that a service is installed and running.
--
-- Assumes that there is a 1:1 mapping between service names and apt
-- package names.
serviceInstalledRunning :: Package -> Property
serviceInstalledRunning svc = Service.running svc `requires` installed [svc]

data AptKey = AptKey
	{ keyname :: String
	, pubkey :: String
	}

trustsKey :: AptKey -> RevertableProperty
trustsKey k = RevertableProperty trust untrust
  where
	desc = "apt trusts key " ++ keyname k
	f = "/etc/apt/trusted.gpg.d" </> keyname k ++ ".gpg"
	untrust = File.notPresent f
	trust = check (not <$> doesFileExist f) $ property desc $ makeChange $ do
		withHandle StdinHandle createProcessSuccess
			(proc "gpg" ["--no-default-keyring", "--keyring", f, "--import", "-"]) $ \h -> do
				hPutStr h (pubkey k)
				hClose h
		nukeFile $ f ++ "~" -- gpg dropping

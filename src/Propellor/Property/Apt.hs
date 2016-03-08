{-# LANGUAGE FlexibleContexts #-}

module Propellor.Property.Apt where

import Data.Maybe
import Data.List
import System.IO
import Control.Monad
import Control.Applicative
import Prelude

import Propellor.Base
import qualified Propellor.Property.File as File
import qualified Propellor.Property.Service as Service
import Propellor.Property.File (Line)

sourcesList :: FilePath
sourcesList = "/etc/apt/sources.list"

type Url = String
type Section = String

type SourcesGenerator = DebianSuite -> [Line]

showSuite :: DebianSuite -> String
showSuite (Stable s) = s
showSuite Testing = "testing"
showSuite Unstable = "unstable"
showSuite Experimental = "experimental"

backportSuite :: DebianSuite -> Maybe String
backportSuite (Stable s) = Just (s ++ "-backports")
backportSuite _ = Nothing

stableUpdatesSuite :: DebianSuite -> Maybe String
stableUpdatesSuite (Stable s) = Just (s ++ "-updates")
stableUpdatesSuite _ = Nothing

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
binandsrc url suite = catMaybes
	[ Just l
	, Just $ srcLine l
	, bl
	, srcLine <$> bl
	]
  where
	l = debLine (showSuite suite) url stdSections
	bl = do
		bs <- backportSuite suite
		return $ debLine bs url stdSections

debCdn :: SourcesGenerator
debCdn = binandsrc "http://httpredir.debian.org/debian"

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
-- with the Debian suite configured by the os.
--
-- Since the CDN is sometimes unreliable, also adds backup lines using
-- kernel.org.
stdSourcesList :: Property NoInfo
stdSourcesList = withOS "standard sources.list" $ \o ->
	case o of
		(Just (System (Debian suite) _)) ->
			ensureProperty $ stdSourcesListFor suite
		_ -> error "os is not declared to be Debian"

stdSourcesListFor :: DebianSuite -> Property NoInfo
stdSourcesListFor suite = stdSourcesList' suite []

-- | Adds additional sources.list generators.
--
-- Note that if a Property needs to enable an apt source, it's better
-- to do so via a separate file in </etc/apt/sources.list.d/>
stdSourcesList' :: DebianSuite -> [SourcesGenerator] -> Property NoInfo
stdSourcesList' suite more = setSourcesList
	(concatMap (\gen -> gen suite) generators)
	`describe` ("standard sources.list for " ++ show suite)
  where
	generators = [debCdn, kernelOrg, securityUpdates] ++ more

setSourcesList :: [Line] -> Property NoInfo
setSourcesList ls = sourcesList `File.hasContent` ls `onChange` update

setSourcesListD :: [Line] -> FilePath -> Property NoInfo
setSourcesListD ls basename = f `File.hasContent` ls `onChange` update
  where
	f = "/etc/apt/sources.list.d/" ++ basename ++ ".list"

runApt :: [String] -> UncheckedProperty NoInfo
runApt ps = cmdPropertyEnv "apt-get" ps noninteractiveEnv

noninteractiveEnv :: [(String, String)]
noninteractiveEnv =
		[ ("DEBIAN_FRONTEND", "noninteractive")
		, ("APT_LISTCHANGES_FRONTEND", "none")
		]

update :: Property NoInfo
update = runApt ["update"]
	`assume` MadeChange
	`describe` "apt update"

-- | Have apt upgrade packages, adding new packages and removing old as
-- necessary.
upgrade :: Property NoInfo
upgrade = upgrade' "dist-upgrade"

upgrade' :: String -> Property NoInfo
upgrade' p = combineProperties ("apt " ++ p)
	[ pendingConfigured
	, runApt ["-y", p]
		`assume` MadeChange
	]

-- | Have apt upgrade packages, but never add new packages or remove
-- old packages. Not suitable for upgrading acrocess major versions
-- of the distribution.
safeUpgrade :: Property NoInfo
safeUpgrade = upgrade' "upgrade"

-- | Have dpkg try to configure any packages that are not fully configured.
pendingConfigured :: Property NoInfo
pendingConfigured = cmdPropertyEnv "dpkg" ["--configure", "--pending"] noninteractiveEnv
	`assume` MadeChange
	`describe` "dpkg configured pending"

type Package = String

installed :: [Package] -> Property NoInfo
installed = installed' ["-y"]

installed' :: [String] -> [Package] -> Property NoInfo
installed' params ps = robustly $ check (isInstallable ps) go
	`describe` unwords ("apt installed":ps)
  where
	go = runApt (params ++ ["install"] ++ ps)

installedBackport :: [Package] -> Property NoInfo
installedBackport ps = withOS desc $ \o -> case o of
	(Just (System (Debian suite) _)) -> case backportSuite suite of
		Nothing -> unsupportedOS
		Just bs -> ensureProperty $
			runApt (["install", "-t", bs, "-y"] ++ ps)
				`changesFile` dpkgStatus
	_ -> unsupportedOS
  where
	desc = unwords ("apt installed backport":ps)

-- | Minimal install of package, without recommends.
installedMin :: [Package] -> Property NoInfo
installedMin = installed' ["--no-install-recommends", "-y"]

removed :: [Package] -> Property NoInfo
removed ps = check (or <$> isInstalled' ps) (runApt (["-y", "remove"] ++ ps))
	`describe` unwords ("apt removed":ps)

buildDep :: [Package] -> Property NoInfo
buildDep ps = robustly $ go
	`changesFile` dpkgStatus
	`describe` unwords ("apt build-dep":ps)
  where
	go = runApt $ ["-y", "build-dep"] ++ ps

-- | Installs the build deps for the source package unpacked
-- in the specifed directory, with a dummy package also
-- installed so that autoRemove won't remove them.
buildDepIn :: FilePath -> Property NoInfo
buildDepIn dir = cmdPropertyEnv "sh" ["-c", cmd] noninteractiveEnv
	`changesFile` dpkgStatus
	`requires` installedMin ["devscripts", "equivs"]
  where
	cmd = "cd '" ++ dir ++ "' && mk-build-deps debian/control --install --tool 'apt-get -y --no-install-recommends' --remove"

-- | Package installation may fail becuse the archive has changed.
-- Run an update in that case and retry.
robustly :: (Combines (Property i) (Property NoInfo)) => Property i -> Property i
robustly p = adjustPropertySatisfy p $ \satisfy -> do
	r <- satisfy
	if r == FailedChange
		-- Safe to use ignoreInfo because we're re-running
		-- the same property.
		then ensureProperty $ ignoreInfo $ p `requires` update
		else return r

isInstallable :: [Package] -> IO Bool
isInstallable ps = do
	l <- isInstalled' ps
	return $ elem False l && not (null l)

isInstalled :: Package -> IO Bool
isInstalled p = (== [True]) <$> isInstalled' [p]

-- | Note that the order of the returned list will not always
-- correspond to the order of the input list. The number of items may
-- even vary. If apt does not know about a package at all, it will not
-- be included in the result list.
isInstalled' :: [Package] -> IO [Bool]
isInstalled' ps = (mapMaybe parse . lines) <$> policy
  where
	parse l
		| "Installed: (none)" `isInfixOf` l = Just False
		| "Installed: " `isInfixOf` l = Just True
		| otherwise = Nothing
	policy = do
		environ <- addEntry "LANG" "C" <$> getEnvironment
		readProcessEnv "apt-cache" ("policy":ps) (Just environ)

autoRemove :: Property NoInfo
autoRemove = runApt ["-y", "autoremove"]
	`changesFile` dpkgStatus
	`describe` "apt autoremove"

-- | Enables unattended upgrades. Revert to disable.
unattendedUpgrades :: RevertableProperty NoInfo
unattendedUpgrades = enable <!> disable
  where
	enable = setup True
		`before` Service.running "cron"
		`before` configure
		-- work around http://bugs.debian.org/812380
		`before` File.notPresent "/etc/apt/apt.conf.d/50unattended-upgrades.ucf-dist"
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
					("Unattended-Upgrade::Origins-Pattern { \"o=Debian,a="++showSuite suite++"\"; };")
			_ -> noChange

type DebconfTemplate = String
type DebconfTemplateType = String
type DebconfTemplateValue = String

-- | Preseeds debconf values and reconfigures the package so it takes
-- effect.
reConfigure :: Package -> [(DebconfTemplate, DebconfTemplateType, DebconfTemplateValue)] -> Property NoInfo
reConfigure package vals = reconfigure `requires` setselections
	`describe` ("reconfigure " ++ package)
  where
	setselections = property "preseed" $
		if null vals
			then noChange
			else makeChange $
				withHandle StdinHandle createProcessSuccess
					(proc "debconf-set-selections" []) $ \h -> do
						forM_ vals $ \(tmpl, tmpltype, value) ->
							hPutStrLn h $ unwords [package, tmpl, tmpltype, value]
						hClose h
	reconfigure = cmdPropertyEnv "dpkg-reconfigure" ["-fnone", package] noninteractiveEnv
		`assume` MadeChange

-- | Ensures that a service is installed and running.
--
-- Assumes that there is a 1:1 mapping between service names and apt
-- package names.
serviceInstalledRunning :: Package -> Property NoInfo
serviceInstalledRunning svc = Service.running svc `requires` installed [svc]

data AptKey = AptKey
	{ keyname :: String
	, pubkey :: String
	}

trustsKey :: AptKey -> RevertableProperty NoInfo
trustsKey k = trustsKey' k <!> untrustKey k

trustsKey' :: AptKey -> Property NoInfo
trustsKey' k = check (not <$> doesFileExist f) $ property desc $ makeChange $ do
	withHandle StdinHandle createProcessSuccess
		(proc "gpg" ["--no-default-keyring", "--keyring", f, "--import", "-"]) $ \h -> do
			hPutStr h (pubkey k)
			hClose h
	nukeFile $ f ++ "~" -- gpg dropping
  where
	desc = "apt trusts key " ++ keyname k
	f = aptKeyFile k

untrustKey :: AptKey -> Property NoInfo
untrustKey = File.notPresent . aptKeyFile

aptKeyFile :: AptKey -> FilePath
aptKeyFile k = "/etc/apt/trusted.gpg.d" </> keyname k ++ ".gpg"

-- | Cleans apt's cache of downloaded packages to avoid using up disk
-- space.
cacheCleaned :: Property NoInfo
cacheCleaned = cmdProperty "apt-get" ["clean"]
	`assume` NoChange
	`describe` "apt cache cleaned"

-- | Add a foreign architecture to dpkg and apt.
hasForeignArch :: String -> Property NoInfo
hasForeignArch arch = check notAdded (add `before` update)
	`describe` ("dpkg has foreign architecture " ++ arch)
  where
	notAdded = (notElem arch . lines) <$> readProcess "dpkg" ["--print-foreign-architectures"]
	add = cmdProperty "dpkg" ["--add-architecture", arch]
		`assume` MadeChange

dpkgStatus :: FilePath
dpkgStatus = "/var/lib/dpkg/status"

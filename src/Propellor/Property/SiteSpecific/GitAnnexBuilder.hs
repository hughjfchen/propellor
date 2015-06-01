{-# LANGUAGE FlexibleContexts #-}

module Propellor.Property.SiteSpecific.GitAnnexBuilder where

import Propellor
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.User as User
import qualified Propellor.Property.Cron as Cron
import qualified Propellor.Property.File as File
import qualified Propellor.Property.Systemd as Systemd
import qualified Propellor.Property.Chroot as Chroot
import Propellor.Property.Cron (Times)

builduser :: UserName
builduser = "builder"

homedir :: FilePath
homedir = "/home/builder"

gitbuilderdir :: FilePath
gitbuilderdir = homedir </> "gitbuilder"

builddir :: FilePath
builddir = gitbuilderdir </> "build"

type TimeOut = String -- eg, 5h

autobuilder :: Architecture -> Times -> TimeOut -> Property HasInfo
autobuilder arch crontimes timeout = combineProperties "gitannexbuilder" $ props
	& Apt.serviceInstalledRunning "cron"
	& Cron.niceJob "gitannexbuilder" crontimes (User builduser) gitbuilderdir
		("git pull ; timeout " ++ timeout ++ " ./autobuild")
	& rsyncpassword
  where
	context = Context ("gitannexbuilder " ++ arch)
	pwfile = homedir </> "rsyncpassword"
	-- The builduser account does not have a password set,
	-- instead use the password privdata to hold the rsync server
	-- password used to upload the built image.
	rsyncpassword = withPrivData (Password builduser) context $ \getpw ->
		property "rsync password" $ getpw $ \pw -> do
			oldpw <- liftIO $ catchDefaultIO "" $
				readFileStrict pwfile
			if pw /= oldpw
				then makeChange $ writeFile pwfile pw
				else noChange

tree :: Architecture -> Property HasInfo
tree buildarch = combineProperties "gitannexbuilder tree" $ props
	& Apt.installed ["git"]
	& File.dirExists gitbuilderdir
	& File.ownerGroup gitbuilderdir (User builduser) (Group builduser)
	& gitannexbuildercloned
	& builddircloned
  where
	gitannexbuildercloned = check (not <$> (doesDirectoryExist (gitbuilderdir </> ".git"))) $ 
		userScriptProperty (User builduser)
			[ "git clone git://git.kitenet.net/gitannexbuilder " ++ gitbuilderdir
			, "cd " ++ gitbuilderdir
			, "git checkout " ++ buildarch
			]
			`describe` "gitbuilder setup"
	builddircloned = check (not <$> doesDirectoryExist builddir) $ userScriptProperty (User builduser)
		[ "git clone git://git-annex.branchable.com/ " ++ builddir
		]

buildDepsApt :: Property HasInfo
buildDepsApt = combineProperties "gitannexbuilder build deps" $ props
	& Apt.buildDep ["git-annex"]
	& Apt.installed ["liblockfile-simple-perl"]
	& buildDepsNoHaskellLibs
	& Apt.buildDepIn builddir
		`describe` "git-annex source build deps installed"

buildDepsNoHaskellLibs :: Property NoInfo
buildDepsNoHaskellLibs = Apt.installed
	["git", "rsync", "moreutils", "ca-certificates",
	"debhelper", "ghc", "curl", "openssh-client", "git-remote-gcrypt",
	"liblockfile-simple-perl", "cabal-install", "vim", "less",
	-- needed by haskell libs
	"libxml2-dev", "libidn11-dev", "libgsasl7-dev", "libgnutls28-dev",
	"alex", "happy", "c2hs"
	]

haskellPkgsInstalled :: String -> Property NoInfo
haskellPkgsInstalled dir = flagFile go ("/haskellpkgsinstalled")
  where
	go = userScriptProperty (User builduser)
		[ "cd " ++ builddir ++ " && ./standalone/ " ++ dir ++ "/install-haskell-packages"
		]

-- Installs current versions of git-annex's deps from cabal, but only
-- does so once.
cabalDeps :: Property NoInfo
cabalDeps = flagFile go cabalupdated
	where
		go = userScriptProperty (User builduser) ["cabal update && cabal install git-annex --only-dependencies || true"]
		cabalupdated = homedir </> ".cabal" </> "packages" </> "hackage.haskell.org" </> "00-index.cache"

autoBuilderContainer :: (System -> Property HasInfo) -> System -> Times -> TimeOut -> Systemd.Container
autoBuilderContainer mkprop osver@(System _ arch) crontime timeout =
	Systemd.container name bootstrap
		& mkprop osver
		& buildDepsApt
		& autobuilder arch crontime timeout
  where
	name = arch ++ "-git-annex-builder"
	bootstrap = Chroot.debootstrapped osver mempty

standardAutoBuilder :: System -> Property HasInfo
standardAutoBuilder osver@(System _ arch) =
	propertyList "standard git-annex autobuilder" $ props
		& os osver
		& Apt.stdSourcesList
		& Apt.unattendedUpgrades
		& User.accountFor (User builduser)
		& tree arch

armAutoBuilder :: System -> Times -> TimeOut -> Property HasInfo
armAutoBuilder osver@(System _ arch) crontime timeout = 
	propertyList "arm git-annex autobuilder" $ props
		& standardAutoBuilder osver
		& buildDepsNoHaskellLibs
		-- Works around ghc crash with parallel builds on arm.
		& (homedir </> ".cabal" </> "config")
			`File.lacksLine` "jobs: $ncpus"
		-- Install patched haskell packages for portability to
		-- arm NAS's using old kernel versions.
		& haskellPkgsInstalled "linux"
		& autobuilder arch crontime timeout

androidAutoBuilderContainer :: Times -> TimeOut -> Systemd.Container
androidAutoBuilderContainer crontimes timeout =
	androidContainer "android-git-annex-builder" (tree "android") builddir
		& Apt.unattendedUpgrades
		& autobuilder "android" crontimes timeout

-- Android is cross-built in a Debian i386 container, using the Android NDK.
androidContainer
	:: (IsProp (Property (CInfo NoInfo i)), (Combines (Property NoInfo) (Property i)))
	=> Systemd.MachineName
	-> Property i
	-> FilePath
	-> Systemd.Container
androidContainer name setupgitannexdir gitannexdir = Systemd.container name bootstrap
	& os osver
	& Apt.stdSourcesList
	& User.accountFor (User builduser)
	& File.dirExists gitbuilderdir
	& File.ownerGroup homedir (User builduser) (Group builduser)
	& flagFile chrootsetup ("/chrootsetup")
		`requires` setupgitannexdir
	& buildDepsApt
	& haskellPkgsInstalled "android"
  where
	-- Use git-annex's android chroot setup script, which will install
	-- ghc-android and the NDK, all build deps, etc, in the home
	-- directory of the builder user.
	chrootsetup = scriptProperty
		[ "cd " ++ gitannexdir ++ " && ./standalone/android/buildchroot-inchroot"
		]
	osver = System (Debian Testing) "i386"
	bootstrap = Chroot.debootstrapped osver mempty

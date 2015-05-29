{-# LANGUAGE FlexibleContexts #-}

module Propellor.Property.SiteSpecific.GitAnnexBuilder where

import Propellor
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.User as User
import qualified Propellor.Property.Cron as Cron
import qualified Propellor.Property.Ssh as Ssh
import qualified Propellor.Property.File as File
import qualified Propellor.Property.Docker as Docker
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
	-- gitbuilderdir directory already exists when docker volume is used,
	-- but with wrong owner.
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

-- Installs current versions of git-annex's deps from cabal, but only
-- does so once.
cabalDeps :: Property NoInfo
cabalDeps = flagFile go cabalupdated
	where
		go = userScriptProperty (User builduser) ["cabal update && cabal install git-annex --only-dependencies || true"]
		cabalupdated = homedir </> ".cabal" </> "packages" </> "hackage.haskell.org" </> "00-index.cache"

standardAutoBuilderContainer :: Architecture -> Int -> TimeOut -> Systemd.Container
standardAutoBuilderContainer arch buildminute timeout = Systemd.container name bootstrap
	& os osver
	& Apt.stdSourcesList
	& Apt.unattendedUpgrades
	& User.accountFor (User builduser)
	& tree arch
	& buildDepsApt
	& autobuilder arch (Cron.Times $ show buildminute ++ " * * * *") timeout
  where
	name = arch ++ "-git-annex-builder"
	bootstrap = Chroot.debootstrapped osver mempty
	osver = System (Debian Testing) arch

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
	& flagFile haskellpkgsinstalled ("/haskellpkgsinstalled")
  where
	-- Use git-annex's android chroot setup script, which will install
	-- ghc-android and the NDK, all build deps, etc, in the home
	-- directory of the builder user.
	chrootsetup = scriptProperty
		[ "cd " ++ gitannexdir ++ " && ./standalone/android/buildchroot-inchroot"
		]
	haskellpkgsinstalled = userScriptProperty (User builduser)
		[ "cd " ++ gitannexdir ++ " && ./standalone/android/install-haskell-packages"
		]
	osver = System (Debian Testing) "i386"
	bootstrap = Chroot.debootstrapped osver mempty

-- armel builder has a companion container using amd64 that
-- runs the build first to get TH splices. They need
-- to have the same versions of all haskell libraries installed.
armelCompanionContainer :: (System -> Docker.Image) -> Docker.Container
armelCompanionContainer dockerImage = Docker.container "armel-git-annex-builder-companion"
	(dockerImage $ System (Debian Unstable) "amd64")
	& os (System (Debian Testing) "amd64")
	& Apt.stdSourcesList
	& Apt.installed ["systemd"]
	-- This volume is shared with the armel builder.
	& Docker.volume gitbuilderdir
	& User.accountFor (User builduser)
	-- Install current versions of build deps from cabal.
	& tree "armel"
	& buildDepsNoHaskellLibs
	& cabalDeps
	-- The armel builder can ssh to this companion.
	& Docker.expose "22"
	& Apt.serviceInstalledRunning "ssh"
	& Ssh.authorizedKeys (User builduser) (Context "armel-git-annex-builder")
	& Docker.tweaked

armelAutoBuilderContainer :: (System -> Docker.Image) -> Times -> TimeOut -> Docker.Container
armelAutoBuilderContainer dockerImage crontimes timeout = Docker.container "armel-git-annex-builder"
	(dockerImage $ System (Debian Unstable) "armel")
	& os (System (Debian Testing) "armel")
	& Apt.stdSourcesList
	& Apt.installed ["systemd"]
	& Apt.installed ["openssh-client"]
	& Docker.link "armel-git-annex-builder-companion" "companion"
	& Docker.volumes_from "armel-git-annex-builder-companion"
	& User.accountFor (User builduser)
	-- TODO: automate installing haskell libs
	-- (Currently have to run
	-- git-annex/standalone/linux/install-haskell-packages
	-- which is not fully automated.)
	& buildDepsNoHaskellLibs
	& autobuilder "armel" crontimes timeout
		`requires` tree "armel"
	& Ssh.keyImported SshRsa (User builduser) (Context "armel-git-annex-builder")
	& trivial writecompanionaddress
	& Docker.tweaked
  where
	writecompanionaddress = scriptProperty
		[ "echo \"$COMPANION_PORT_22_TCP_ADDR\" > " ++ homedir </> "companion_address"
		] `describe` "companion_address file"

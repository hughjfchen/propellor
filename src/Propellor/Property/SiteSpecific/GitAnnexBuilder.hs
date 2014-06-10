module Propellor.Property.SiteSpecific.GitAnnexBuilder where

import Propellor
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.User as User
import qualified Propellor.Property.Cron as Cron
import qualified Propellor.Property.Ssh as Ssh
import qualified Propellor.Property.File as File
import qualified Propellor.Property.Docker as Docker
import Propellor.Property.Cron (CronTimes)

builduser :: UserName
builduser = "builder"

homedir :: FilePath
homedir = "/home/builder"

gitbuilderdir :: FilePath
gitbuilderdir = homedir </> "gitbuilder"

builddir :: FilePath
builddir = gitbuilderdir </> "build"

type TimeOut = String -- eg, 5h

autobuilder :: CronTimes -> TimeOut -> Bool -> Property
autobuilder crontimes timeout rsyncupload = combineProperties "gitannexbuilder"
	[ Apt.serviceInstalledRunning "cron"
	, Cron.niceJob "gitannexbuilder" crontimes builduser gitbuilderdir $
		"git pull ; timeout " ++ timeout ++ " ./autobuild"
	-- The builduser account does not have a password set,
	-- instead use the password privdata to hold the rsync server
	-- password used to upload the built image.
	, property "rsync password" $ do
		let f = homedir </> "rsyncpassword"
		if rsyncupload 
			then withPrivData (Password builduser) $ \p -> do
				oldp <- liftIO $ catchDefaultIO "" $
					readFileStrict f
				if p /= oldp
					then makeChange $ writeFile f p
					else noChange
			else do
				ifM (liftIO $ doesFileExist f)
					( noChange
					, makeChange $ writeFile f "no password configured"
					)
	]

tree :: Architecture -> Property
tree buildarch = combineProperties "gitannexbuilder tree"
	[ Apt.installed ["git"]
	-- gitbuilderdir directory already exists when docker volume is used,
	-- but with wrong owner.
	, File.dirExists gitbuilderdir
	, File.ownerGroup gitbuilderdir builduser builduser
	, check (not <$> (doesDirectoryExist (gitbuilderdir </> ".git"))) $ 
		userScriptProperty builduser
			[ "git clone git://git.kitenet.net/gitannexbuilder " ++ gitbuilderdir
			, "cd " ++ gitbuilderdir
			, "git checkout " ++ buildarch
			]
			`describe` "gitbuilder setup"
	, check (not <$> doesDirectoryExist builddir) $ userScriptProperty builduser
		[ "git clone git://git-annex.branchable.com/ " ++ builddir
		]
	]

buildDepsApt :: Property
buildDepsApt = combineProperties "gitannexbuilder build deps"
	[ Apt.buildDep ["git-annex"]
	, buildDepsNoHaskellLibs
	, "git-annex source build deps installed" ==> Apt.buildDepIn builddir
	]

buildDepsNoHaskellLibs :: Property
buildDepsNoHaskellLibs = Apt.installed
	["git", "rsync", "moreutils", "ca-certificates",
	"debhelper", "ghc", "curl", "openssh-client", "git-remote-gcrypt",
	"liblockfile-simple-perl", "cabal-install", "vim", "less",
	-- needed by haskell libs
	"libxml2-dev", "libidn11-dev", "libgsasl7-dev", "libgnutls-dev",
	"alex", "happy", "c2hs"
	]

-- Installs current versions of git-annex's deps from cabal, but only
-- does so once.
cabalDeps :: Property
cabalDeps = flagFile go cabalupdated
	where
		go = userScriptProperty builduser ["cabal update && cabal install git-annex --only-dependencies || true"]
		cabalupdated = homedir </> ".cabal" </> "packages" </> "hackage.haskell.org" </> "00-index.cache"

standardAutoBuilderContainer :: (System -> Docker.Image) -> Architecture -> Int -> TimeOut -> Host
standardAutoBuilderContainer dockerImage arch buildminute timeout = Docker.container (arch ++ "-git-annex-builder")
	(dockerImage $ System (Debian Testing) arch)
	& os (System (Debian Testing) arch)
	& Apt.stdSourcesList
	& Apt.installed ["systemd"]
	& Apt.unattendedUpgrades
	& User.accountFor builduser
	& tree arch
	& buildDepsApt
	& autobuilder (show buildminute ++ " * * * *") timeout True

androidAutoBuilderContainer :: (System -> Docker.Image) -> Cron.CronTimes -> TimeOut -> Host
androidAutoBuilderContainer dockerImage crontimes timeout =
	androidContainer dockerImage "android-git-annex-builder" (tree "android") builddir
		& Apt.unattendedUpgrades
		& autobuilder crontimes timeout True

-- Android is cross-built in a Debian i386 container, using the Android NDK.
androidContainer :: (System -> Docker.Image) -> Docker.ContainerName -> Property -> FilePath -> Host
androidContainer dockerImage name setupgitannexdir gitannexdir = Docker.container name
	(dockerImage $ System (Debian Stable) "i386")
	& os (System (Debian Stable) "i386")
	& Apt.stdSourcesList
	& Apt.installed ["systemd"]
	& User.accountFor builduser
	& File.dirExists gitbuilderdir
	& File.ownerGroup homedir builduser builduser
	& buildDepsNoHaskellLibs
	& flagFile chrootsetup ("/chrootsetup")
		`requires` setupgitannexdir
	-- TODO: automate installing haskell libs
	-- (Currently have to run
	-- git-annex/standalone/android/install-haskell-packages
	-- which is not fully automated.)
  where
	-- Use git-annex's android chroot setup script, which will install
	-- ghc-android and the NDK, all build deps, etc, in the home
	-- directory of the builder user.
	chrootsetup = scriptProperty
		[ "cd " ++ gitannexdir ++ " && ./standalone/android/buildchroot-inchroot"
		]

-- armel builder has a companion container using amd64 that
-- runs the build first to get TH splices. They need
-- to have the same versions of all haskell libraries installed.
armelCompanionContainer :: (System -> Docker.Image) -> Host
armelCompanionContainer dockerImage = Docker.container "armel-git-annex-builder-companion"
	(dockerImage $ System (Debian Unstable) "amd64")
	& os (System (Debian Testing) "amd64")
	& Apt.stdSourcesList
	& Apt.installed ["systemd"]
	& Apt.unattendedUpgrades
	-- This volume is shared with the armel builder.
	& Docker.volume gitbuilderdir
	& User.accountFor builduser
	-- Install current versions of build deps from cabal.
	& tree "armel"
	& buildDepsNoHaskellLibs
	& cabalDeps
	-- The armel builder can ssh to this companion.
	& Docker.expose "22"
	& Apt.serviceInstalledRunning "ssh"
	& Ssh.authorizedKeys builduser

armelAutoBuilderContainer :: (System -> Docker.Image) -> Cron.CronTimes -> TimeOut -> Host
armelAutoBuilderContainer dockerImage crontimes timeout = Docker.container "armel-git-annex-builder"
	(dockerImage $ System (Debian Unstable) "armel")
	& os (System (Debian Testing) "armel")
	& Apt.stdSourcesList
	& Apt.unattendedUpgrades
	& Apt.installed ["systemd"]
	& Apt.installed ["openssh-client"]
	& Docker.link "armel-git-annex-builder-companion" "companion"
	& Docker.volumes_from "armel-git-annex-builder-companion"
	& User.accountFor builduser
	-- TODO: automate installing haskell libs
	-- (Currently have to run
	-- git-annex/standalone/linux/install-haskell-packages
	-- which is not fully automated.)
	& buildDepsNoHaskellLibs
	& autobuilder crontimes timeout True
		`requires` tree "armel"
	& Ssh.keyImported SshRsa builduser
	& trivial writecompanionaddress
  where
	writecompanionaddress = scriptProperty
		[ "echo \"$COMPANION_PORT_22_TCP_ADDR\" > " ++ homedir </> "companion_address"
		] `describe` "companion_address file"

module Propellor.Property.SiteSpecific.GitAnnexBuilder where

import Propellor
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.User as User
import qualified Propellor.Property.Cron as Cron
import qualified Propellor.Property.File as File
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

builder :: Architecture -> CronTimes -> TimeOut -> Bool -> Property
builder = builder' buildDeps

builder' :: Property -> Architecture -> CronTimes -> TimeOut -> Bool -> Property
builder' buildepsprop buildarch crontimes timeout rsyncupload = combineProperties "gitannexbuilder"
	[ tree buildarch
	, buildepsprop
	, Apt.serviceInstalledRunning "cron"
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
	[ User.accountFor builduser
	-- Home directory already exists when docker volume is used.
	, File.ownerGroup homedir builduser builduser
	, Apt.installed ["git"]
	, check (not <$> doesDirectoryExist gitbuilderdir) $ userScriptProperty builduser
		[ "git clone git://git.kitenet.net/gitannexbuilder " ++ gitbuilderdir
		, "cd " ++ gitbuilderdir
		, "git checkout " ++ buildarch
		]
		`describe` "gitbuilder setup"
	, check (not <$> doesDirectoryExist builddir) $ userScriptProperty builduser
		[ "git clone git://git-annex.branchable.com/ " ++ builddir
		]
	]

buildDeps :: Property
buildDeps = combineProperties "gitannexbuilder build deps"
	[ Apt.buildDep ["git-annex"]
	, buildDepsNoHaskellLibs
	, "git-annex source build deps installed" ==> Apt.buildDepIn builddir
	]

buildDepsNoHaskellLibs :: Property
buildDepsNoHaskellLibs = Apt.installed ["git", "rsync", "moreutils", "ca-certificates",
	"debhelper", "ghc", "curl", "openssh-client", "git-remote-gcrypt",
	"liblockfile-simple-perl", "cabal-install", "vim", "less",
	"alex", "happy", "c2hs",
	-- these haskell libs depend on C libs and don't use TH
	"libghc-dbus-dev", "libghc-fdo-notify-dev", "libghc-network-protocol-xmpp-dev"
	]

-- Installs current versions of git-annex's deps from cabal, but only
-- does so once.
cabalDeps :: Property
cabalDeps = flagFile go cabalupdated
	where
		go = userScriptProperty builduser ["cabal update && cabal install git-annex --only-dependencies || true"]
		cabalupdated = homedir </> ".cabal" </> "packages" </> "hackage.haskell.org" </> "00-index.cache"

-- Ensure a ssh key is set up, and allow it to be used to ssh in
sshKeyGen :: Property
sshKeyGen = combineProperties "sshkeygen"
	[ flagFile gen privkey
	, flagFile auth authkeys
	]
  where
  	gen = userScriptProperty builduser ["ssh-keygen -t RSA -N '' -f " ++ privkey]
	auth = userScriptProperty builduser ["cp " ++ pubkey ++ " " ++ authkeys]
	privkey = homedir </> ".ssh" </> "id_rsa"
	pubkey = privkey ++ ".pub"
	authkeys = homedir </> ".ssh" </> "authorized_keys"

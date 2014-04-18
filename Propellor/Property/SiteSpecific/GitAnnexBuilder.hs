module Propellor.Property.SiteSpecific.GitAnnexBuilder where

import Propellor
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.User as User
import qualified Propellor.Property.Cron as Cron
import Propellor.Property.Cron (CronTimes)

builduser :: UserName
builduser = "builder"

homedir :: FilePath
homedir = "/home/builder"

gitbuilderdir :: FilePath
gitbuilderdir = homedir </> "gitbuilder"

builddir :: FilePath
builddir = gitbuilderdir </> "build"

builder :: Architecture -> CronTimes -> Bool -> Property
builder arch crontimes rsyncupload = combineProperties "gitannexbuilder"
	[ Apt.stdSourcesList Unstable
	, Apt.buildDep ["git-annex"]
	, Apt.installed ["git", "rsync", "moreutils", "ca-certificates",
		"liblockfile-simple-perl", "cabal-install", "vim", "less"]
	, Apt.serviceInstalledRunning "cron"
	, User.accountFor builduser
	, check (not <$> doesDirectoryExist gitbuilderdir) $ userScriptProperty builduser
		[ "git clone git://git.kitenet.net/gitannexbuilder " ++ gitbuilderdir
		, "cd " ++ gitbuilderdir
		, "git checkout " ++ arch
		]
		`describe` "gitbuilder setup"
	, check (not <$> doesDirectoryExist builddir) $ userScriptProperty builduser
		[ "git clone git://git-annex.branchable.com/ " ++ builddir
		]
	, "git-annex source build deps installed" ==> Apt.buildDepIn builddir
	, Cron.niceJob "gitannexbuilder" crontimes builduser gitbuilderdir "git pull ; ./autobuild"
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

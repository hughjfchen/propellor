module Propellor.Property.SiteSpecific.GitAnnexBuilder where

import Propellor
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.User as User
import qualified Propellor.Property.Cron as Cron
import Propellor.Property.Cron (CronTimes)

builduser :: UserName
builduser = "builder"

builddir :: FilePath
builddir = "gitbuilder"

builder :: Architecture -> CronTimes -> Bool -> Property
builder arch crontimes rsyncupload = combineProperties "gitannexbuilder"
	[ Apt.stdSourcesList Unstable
	, Apt.buildDep ["git-annex"]
	, Apt.installed ["git", "rsync", "moreutils", "ca-certificates",
		"liblockfile-simple-perl", "cabal-install", "vim", "less",
		"libghc-fdo-notify-dev"]
	, serviceRunning "cron" `requires` Apt.installed ["cron"]
	, User.accountFor builduser
	, check (lacksdir builddir) $ userScriptProperty builduser
		[ "git clone git://git.kitenet.net/gitannexbuilder " ++ builddir
		, "cd " ++ builddir
		, "git checkout " ++ arch
		]
		`describe` "gitbuilder setup"
	, check (lacksdir $ builddir </> "build") $ userScriptProperty builduser
		[ "cd " ++ builddir
		, "git clone git://git-annex.branchable.com/ build"
		]
	, Cron.niceJob "gitannexbuilder" crontimes builduser ("~/" ++ builddir) "git pull ; ./autobuild"
	-- The builduser account does not have a password set,
	-- instead use the password privdata to hold the rsync server
	-- password used to upload the built image.
	, Property "rsync password" $ do
		d <- homedir
		let f = d </> "rsyncpassword"
		if rsyncupload 
			then withPrivData (Password builduser) $ \p -> do
				oldp <- catchDefaultIO "" $ readFileStrict f
				if p /= oldp
					then makeChange $ writeFile f p
					else noChange
			else do
				ifM (doesFileExist f)
					( noChange
					, makeChange $ writeFile f "no password configured"
					)
	]
  where
  	homedir = fromMaybe ("/home/" ++ builduser) <$> User.homedir builduser
	lacksdir d = do
		h <- homedir
		not <$> doesDirectoryExist (h </> d)

module Propellor.Property.SiteSpecific.GitAnnexBuilder where

import Propellor
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.User as User
import Propellor.Property.Cron (CronTimes)

type Arch = String

builduser :: UserName
builduser = "builder"

builddir :: FilePath
builddir = "gitbuilder"

builder :: Arch -> CronTimes -> Property
builder arch crontimes = combineProperties
	[ Apt.buildDep ["git-annex"]
	, Apt.installed ["git", "rsync", "liblockfile-simple-perl"]
	, serviceRunning "cron" `requires` Apt.installed ["cron"]
	, User.accountFor builduser
	, check (not <$> hasbuilddir) $ userScriptProperty builduser
		[ "cabal update"
		, "git clone https://github.com/joeyh/gitbuilder/"
		, "cd gitbuilder && git checkout " ++ arch
		, "echo '"++crontimes++" cd gitbuilder/autobuild' | crontab -"
		]
		`describe` "gitbuilder setup"
	-- The builduser account does not have a password set,
	-- instead use the password privdata to hold the rsync server
	-- password used to upload the built image.
	, Property "rsync password" $ do
		d <- homedir
		let f = d </> "rsyncpassword"
		withPrivData (Password builduser) $ \p -> do
			oldp <- catchDefaultIO "" $ readFileStrict f
			if p /= oldp
				then makeChange $ writeFile f p
				else noChange
	]
  where
  	homedir = fromMaybe ("/home/" ++ builduser) <$> User.homedir builduser
	hasbuilddir = do
		d <- homedir
		doesDirectoryExist (d </> builddir)

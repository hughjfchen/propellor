module Propellor.Property.Git where

import Propellor
import Propellor.Property.File
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.Service as Service
import Utility.SafeCommand

import Data.List

-- | Exports all git repos in a directory (that user nobody can read)
-- using git-daemon, run from inetd.
--
-- Note that reverting this property does not remove or stop inetd.
daemonRunning :: FilePath -> RevertableProperty
daemonRunning exportdir = RevertableProperty setup unsetup
  where
	setup = containsLine conf (mkl "tcp4")
		`requires`
		containsLine conf (mkl "tcp6")
		`requires`
		dirExists exportdir
		`requires`
		Apt.serviceInstalledRunning "openbsd-inetd"
		`onChange`
		Service.running "openbsd-inetd"
		`describe` ("git-daemon exporting " ++ exportdir)
	unsetup = lacksLine conf (mkl "tcp4")
		`requires`
		lacksLine conf (mkl "tcp6")
		`onChange`
		Service.reloaded "openbsd-inetd"

	conf = "/etc/inetd.conf"

	mkl tcpv = intercalate "\t"
		[ "git"
		, "stream"
		, tcpv
		, "nowait"
		, "nobody"
		, "/usr/bin/git"
		, "git"
		, "daemon"
		, "--inetd"
		, "--export-all"
		, "--base-path=" ++ exportdir
		, exportdir
		]

installed :: Property
installed = Apt.installed ["git"]

type RepoUrl = String

type Branch = String

-- | Specified git repository is cloned to the specified directory.
--
-- If the firectory exists with some other content, it will be recursively
-- deleted.
--
-- A branch can be specified, to check out.
cloned :: UserName -> RepoUrl -> FilePath -> Maybe Branch -> Property
cloned owner url dir mbranch = check originurl (property desc checkout)
	`requires` installed
  where
	desc = "git cloned " ++ url ++ " to " ++ dir
	gitconfig = dir </> ".git/config"
	originurl = ifM (doesFileExist gitconfig)
		( do
			v <- catchDefaultIO Nothing $ headMaybe . lines <$>
				readProcess "git" ["config", "--file", gitconfig, "remote.origin.url"]
			return (v /= Just url)
		, return True
		)
	checkout = do
		liftIO $ do
			whenM (doesDirectoryExist dir) $
				removeDirectoryRecursive dir
			createDirectoryIfMissing True (takeDirectory dir)
		ensureProperty $ userScriptProperty owner $ catMaybes
			-- The </dev/null fixes an intermittent
			-- "fatal: read error: Bad file descriptor"
			-- when run across ssh with propellor --spin
			[ Just $ "git clone " ++ shellEscape url ++ " " ++ shellEscape dir ++ " < /dev/null"
			, Just $ "cd " ++ shellEscape dir
			, ("git checkout " ++) <$> mbranch
			-- In case this repo is exposted via the web,
			-- although the hook to do this ongoing is not
			-- installed here.
			, Just "git update-server-info"
			]

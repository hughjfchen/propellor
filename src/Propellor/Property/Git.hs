module Propellor.Property.Git where

import Propellor
import Propellor.Property.File
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.Service as Service

import Data.List

-- | Exports all git repos in a directory (that user nobody can read)
-- using git-daemon, run from inetd.
--
-- Note that reverting this property does not remove or stop inetd.
daemonRunning :: FilePath -> RevertableProperty
daemonRunning exportdir = setup <!> unsetup
  where
	setup = containsLine conf (mkl "tcp4")
		`requires`
		containsLine conf (mkl "tcp6")
		`requires`
		dirExists exportdir
		`requires`
		Apt.serviceInstalledRunning "openbsd-inetd"
		`onChange`
		Service.reloaded "openbsd-inetd"
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

installed :: Property NoInfo
installed = Apt.installed ["git"]

type RepoUrl = String

type Branch = String

-- | Specified git repository is cloned to the specified directory.
--
-- If the directory exists with some other content (either a non-git
-- repository, or a git repository cloned from some other location),
-- it will be recursively deleted first.
--
-- A branch can be specified, to check out.
cloned :: User -> RepoUrl -> FilePath -> Maybe Branch -> Property NoInfo
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

isGitDir :: FilePath -> IO Bool
isGitDir dir = isNothing <$> catchMaybeIO (readProcess "git" ["rev-parse", "--resolve-git-dir", dir])

data GitShared = Shared Group | SharedAll | NotShared

bareRepo :: FilePath -> User -> GitShared -> Property NoInfo
bareRepo repo user gitshared = check (isRepo repo) $ propertyList ("git repo: " ++ repo) $
	dirExists repo : case gitshared of
		NotShared ->
			[ ownerGroup repo user (userGroup user)
			, userScriptProperty user ["git init --bare --shared=false " ++ shellEscape repo]
			]
		SharedAll ->
			[ ownerGroup repo user (userGroup user)
			, userScriptProperty user ["git init --bare --shared=all " ++ shellEscape repo]
			]
		Shared group' ->
			[ ownerGroup repo user group'
			, userScriptProperty user ["git init --bare --shared=group " ++ shellEscape repo]
			]
  where
	isRepo repo' = isNothing <$> catchMaybeIO (readProcess "git" ["rev-parse", "--resolve-git-dir", repo'])

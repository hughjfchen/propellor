-- | Wrapper program for propellor distribution.
--
-- Distributions should install this program into PATH.
-- (Cabal builds it as dist/build/propellor/propellor).
--
-- This is not the propellor main program (that's config.hs)
--
-- This installs propellor's source into ~/.propellor,
-- uses it to build the real propellor program (if not already built),
-- and runs it.
-- 
-- The source is cloned from /usr/src/propellor when available,
-- or is cloned from git over the network.

module Main where

import Propellor.Message
import Propellor.Bootstrap
import Utility.UserInfo
import Utility.Monad
import Utility.Process
import Utility.SafeCommand
import Utility.Exception

import Control.Monad
import Control.Monad.IfElse
import System.Directory
import System.FilePath
import System.Environment (getArgs)
import System.Exit
import System.Posix.Directory
import System.IO
import Control.Applicative
import Prelude

distdir :: FilePath
distdir = "/usr/src/propellor"

distrepo :: FilePath
distrepo = distdir </> "propellor.git"

disthead :: FilePath
disthead = distdir </> "head"

upstreambranch :: String
upstreambranch = "upstream/master"

-- Using the github mirror of the main propellor repo because
-- it is accessible over https for better security.
netrepo :: String
netrepo = "https://github.com/joeyh/propellor.git"

main :: IO ()
main = withConcurrentOutput $ do
	args <- getArgs
	home <- myHomeDir
	let propellordir = home </> ".propellor"
	let propellorbin = propellordir </> "propellor"
	wrapper args propellordir propellorbin

wrapper :: [String] -> FilePath -> FilePath -> IO ()
wrapper args propellordir propellorbin = do
	ifM (doesDirectoryExist propellordir)
		( checkRepo 
		, makeRepo
		)
	buildruncfg
  where
	makeRepo = do
		putStrLn $ "Setting up your propellor repo in " ++ propellordir
		putStrLn ""
		ifM (doesFileExist distrepo <||> doesDirectoryExist distrepo)
			( do			
				void $ boolSystem "git" [Param "clone", File distrepo, File propellordir]
				fetchUpstreamBranch propellordir distrepo
				changeWorkingDirectory propellordir
				void $ boolSystem "git" [Param "remote", Param "rm", Param "origin"]
			, do
				void $ boolSystem "git" [Param "clone", Param netrepo, File propellordir]
				changeWorkingDirectory propellordir
				-- Rename origin to upstream and avoid
				-- git push to that read-only repo.
				void $ boolSystem "git" [Param "remote", Param "rename", Param "origin", Param "upstream"]
				void $ boolSystem "git" [Param "config", Param "--unset", Param "branch.master.remote", Param "upstream"]
			)

	checkRepo = whenM (doesFileExist disthead <&&> doesFileExist (propellordir </> "propellor.cabal")) $ do
		headrev <- takeWhile (/= '\n') <$> readFile disthead
		changeWorkingDirectory propellordir
		headknown <- catchMaybeIO $ 
			withQuietOutput createProcessSuccess $
				proc "git" ["log", headrev]
		if (headknown == Nothing)
			then setupupstreammaster headrev propellordir
			else do
				merged <- not . null <$>
					readProcess "git" ["log", headrev ++ "..HEAD", "--ancestry-path"]
				unless merged $
					warnoutofdate propellordir True
	buildruncfg = do
		changeWorkingDirectory propellordir
		buildPropellor Nothing
		putStrLn ""
		putStrLn ""
		chain
	chain = do
		(_, _, _, pid) <- createProcess (proc propellorbin args) 
		exitWith =<< waitForProcess pid

-- Passed the user's propellordir repository, makes upstream/master
-- be a usefully mergeable branch.
--
-- We cannot just use origin/master, because in the case of a distrepo,
-- it only contains 1 commit. So, trying to merge with it will result
-- in lots of merge conflicts, since git cannot find a common parent
-- commit.
--
-- Instead, the upstream/master branch is created by taking the
-- upstream/master branch (which must be an old version of propellor,
-- as distributed), and diffing from it to the current origin/master,
-- and committing the result. This is done in a temporary clone of the
-- repository, giving it a new master branch. That new branch is fetched
-- into the user's repository, as if fetching from a upstream remote,
-- yielding a new upstream/master branch.
setupupstreammaster :: String -> FilePath -> IO ()
setupupstreammaster newref propellordir = do
	changeWorkingDirectory propellordir
	go =<< catchMaybeIO getoldrev
  where
	go Nothing = warnoutofdate propellordir False
	go (Just oldref) = do
		let tmprepo = ".git/propellordisttmp"
		let cleantmprepo = void $ catchMaybeIO $ removeDirectoryRecursive tmprepo
		cleantmprepo
		git ["clone", "--quiet", ".", tmprepo]
	
		changeWorkingDirectory tmprepo
		git ["fetch", distrepo, "--quiet"]
		git ["reset", "--hard", oldref, "--quiet"]
		git ["merge", newref, "-s", "recursive", "-Xtheirs", "--quiet", "-m", "merging upstream version"]
	
		fetchUpstreamBranch propellordir tmprepo
		cleantmprepo
		warnoutofdate propellordir True

	getoldrev = takeWhile (/= '\n')
		<$> readProcess "git" ["show-ref", upstreambranch, "--hash"]
	
	git = run "git"
	run cmd ps = unlessM (boolSystem cmd (map Param ps)) $
		error $ "Failed to run " ++ cmd ++ " " ++ show ps

warnoutofdate :: FilePath -> Bool -> IO ()
warnoutofdate propellordir havebranch = do
	warningMessage ("** Your " ++ propellordir ++ " is out of date..")
	let also s = hPutStrLn stderr ("   " ++ s)
	also ("A newer upstream version is available in " ++ distrepo)
	if havebranch
		then also ("To merge it, run: git merge " ++ upstreambranch)
		else also ("To merge it, find the most recent commit in your repository's history that corresponds to an upstream release of propellor, and set refs/remotes/" ++ upstreambranch ++ " to it. Then run propellor again.")
	also ""

fetchUpstreamBranch :: FilePath -> FilePath -> IO ()
fetchUpstreamBranch propellordir repo = do
	changeWorkingDirectory propellordir
	void $ boolSystem "git"
		[ Param "fetch"
		, File repo
		, Param ("+refs/heads/master:refs/remotes/" ++ upstreambranch)
		, Param "--quiet"
		]

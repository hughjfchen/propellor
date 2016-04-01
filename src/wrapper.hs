-- | Wrapper program for propellor distribution.
--
-- Distributions should install this program into PATH.
-- (Cabal builds it as dist/build/propellor/propellor).
--
-- This is not the propellor main program (that's config.hs)
--
-- This bootstraps ~/.propellor/config.hs, builds it if
-- it's not already built, and runs it.

module Main where

import Propellor.Message
import Propellor.Bootstrap
import Propellor.Git
import Utility.UserInfo
import Utility.Monad
import Utility.Process
import Utility.SafeCommand
import Utility.Exception

import Data.Char
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

-- A distribution may include a bundle of propellor's git repository here.
-- If not, it will be pulled from the network when needed.
distrepo :: FilePath
distrepo = distdir </> "propellor.git"

-- File containing the head rev of the distrepo.
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
	let dotpropellor = home </> ".propellor"
	ifM (doesDirectoryExist dotpropellor)
		( do
			checkRepoUpToDate dotpropellor
			buildRunConfig dotpropellor args
		, do
			welcomeBanner
			setup dotpropellor
		)

buildRunConfig :: FilePath -> [String] -> IO ()
buildRunConfig dotpropellor args = do
	changeWorkingDirectory dotpropellor
	buildPropellor Nothing
	putStrLn ""
	putStrLn ""
	chain
  where
	propellorbin = dotpropellor </> "propellor"
	chain = do
		(_, _, _, pid) <- createProcess (proc propellorbin args) 
		exitWith =<< waitForProcess pid

welcomeBanner :: IO ()
welcomeBanner = putStr $ unlines $ map prettify
	[ ""
	, ""
	, "                                 _         ______`|                       ,-.__"
	, " .---------------------------  /   ~___-=O`/|O`/__|                      (____.'"
	, "  - Welcome to              -- ~          / | /    )          _.-'-._"
	, "  -            Propellor!   --  `/-==__ _/__|/__=-|          (       ~_"
	, " `---------------------------   *             ~ | |           '--------'"
	, "                                           (o)  `"
	, ""
	, ""
	]
  where
	prettify = map (replace '~' '\\')
	replace x y c
		| c == x = y
		| otherwise = c

prompt :: String -> [(Char, IO ())] -> IO ()
prompt p cs = do
	putStr (p ++ " [" ++ map fst cs ++ "] ")
	hFlush stdout
	r <- map toLower <$> getLine
	if r == "\n"
		then snd (head cs) -- default to first choice on return
		else case filter (\(c, a) -> [toLower c] == r) cs of
			[(_, a)] -> a
			_ -> do
				putStrLn "Not a valid choice, try again.. (Or ctrl-c to quit)"
				prompt p cs

section :: IO ()
section = do
	putStrLn ""
	putStrLn "---------------------------------------------------------------------------------"
	putStrLn ""

setup :: FilePath -> IO ()
setup dotpropellor = do
	putStrLn "Propellor's configuration file is ~/.propellor/config.hs"
	putStrLn ""
	putStrLn "Lets get you started with a simple config that you can adapt"
	putStrLn "to your needs. You can start with:"
	putStrLn "   A: A clone of propellor's git repository    (most flexible)"
	putStrLn "   B: The bare minimum files to use propellor  (most simple)"
	prompt "Which would you prefer?"
		[ ('A', fullClone dotpropellor),
		 ('B', minimalConfig dotpropellor)
		]
	putStrLn "Ok, ~/.propellor/config.hs is set up!"
	
	section
	putStrLn "Let's try building the propellor configuration, to make sure it will work..."
	buildPropellor Nothing
	putStrLn "Great! Propellor is set up and ready to use."

	section
	putStrLn "Your next step is to edit ~/.propellor/config.hs,"
	putStrLn "and run propellor again to try it out."
	putStrLn ""
	putStrLn "For docs, see https://propellor.branchable.com/"
	putStrLn "Enjoy propellor!"

minimalConfig :: FilePath -> IO ()
minimalConfig dotpropellor = do
	createDirectoryIfMissing True dotpropellor
	writeFile cabalfile (unlines cabalcontent)
	writeFile configfile (unlines configcontent)
	changeWorkingDirectory dotpropellor
	void $ boolSystem "git" [Param "init"]
	void $ boolSystem "git" [Param "add" , File cabalfile, File configfile]
  where
	cabalfile = dotpropellor </> "config.cabal"
	configfile = dotpropellor </> "config.hs"
	cabalcontent =
		[ "-- This is a cabal file to use to build your propellor configuration."
		, ""
		, "Name: config"
		, "Cabal-Version: >= 1.6"
		, "Build-Type: Simple"
		, "Version: 0"
		, ""
		, "Executable propellor-config"
		, "  Main-Is: config.hs"
		, "  GHC-Options: -threaded -Wall -fno-warn-tabs -O0"
		, "  Extensions: TypeOperators"
		, "  Build-Depends: propellor >= 3.0, base >= 3"
		]
	configcontent = 
		[ "-- This is the main configuration file for Propellor, and is used to build"
		, "-- the propellor program."
		, ""
		, "import Propellor"
		, "import qualified Propellor.Property.File as File"
		, "import qualified Propellor.Property.Apt as Apt"
		, "import qualified Propellor.Property.Cron as Cron"
		, "import qualified Propellor.Property.User as User"
		, ""
		, "main :: IO ()"
		, "main = defaultMain hosts"
		, ""
		, "-- The hosts propellor knows about."
		, "hosts :: [Host]"
		, "hosts ="
		, "        [ mybox"
		, "        ]"
		, ""
		, "-- An example host."
		, "mybox :: Host"
		, "mybox = host \"mybox.example.com\" $ props"
		, "        & osDebian Unstable \"amd64\""
		, "        & Apt.stdSourcesList"
		, "        & Apt.unattendedUpgrades"
		, "        & Apt.installed [\"etckeeper\"]"
		, "        & Apt.installed [\"ssh\"]"
		, "        & User.hasSomePassword (User \"root\")"
		, "        & File.dirExists \"/var/www\""
		, "        & Cron.runPropellor (Cron.Times \"30 * * * *\")"
		, ""
		]

fullClone :: FilePath -> IO ()
fullClone dotpropellor = ifM (doesFileExist distrepo <||> doesDirectoryExist distrepo)
	( do			
		void $ boolSystem "git" [Param "clone", File distrepo, File dotpropellor]
		fetchUpstreamBranch dotpropellor distrepo
		changeWorkingDirectory dotpropellor
		void $ boolSystem "git" [Param "remote", Param "rm", Param "origin"]
	, do
		void $ boolSystem "git" [Param "clone", Param netrepo, File dotpropellor]
		changeWorkingDirectory dotpropellor
		-- Rename origin to upstream and avoid
		-- git push to that read-only repo.
		void $ boolSystem "git" [Param "remote", Param "rename", Param "origin", Param "upstream"]
		void $ boolSystem "git" [Param "config", Param "--unset", Param "branch.master.remote", Param "upstream"]
	)

checkRepoUpToDate :: FilePath -> IO ()
checkRepoUpToDate dotpropellor = whenM (gitbundleavail <&&> dotpropellorpopulated) $ do
	headrev <- takeWhile (/= '\n') <$> readFile disthead
	changeWorkingDirectory dotpropellor
	headknown <- catchMaybeIO $ 
		withQuietOutput createProcessSuccess $
			proc "git" ["log", headrev]
	if (headknown == Nothing)
		then setupUpstreamMaster headrev dotpropellor
		else do
			theirhead <- getCurrentGitSha1 =<< getCurrentBranchRef
			when (theirhead /= headrev) $ do
				merged <- not . null <$>
					readProcess "git" ["log", headrev ++ "..HEAD", "--ancestry-path"]
				unless merged $
					warnoutofdate dotpropellor True
  where
	gitbundleavail = doesFileExist disthead
	dotpropellorpopulated = doesFileExist (dotpropellor </> "propellor.cabal")

-- Passed the user's dotpropellor repository, makes upstream/master
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
setupUpstreamMaster :: String -> FilePath -> IO ()
setupUpstreamMaster newref dotpropellor = do
	changeWorkingDirectory dotpropellor
	go =<< catchMaybeIO getoldrev
  where
	go Nothing = warnoutofdate dotpropellor False
	go (Just oldref) = do
		let tmprepo = ".git/propellordisttmp"
		let cleantmprepo = void $ catchMaybeIO $ removeDirectoryRecursive tmprepo
		cleantmprepo
		git ["clone", "--quiet", ".", tmprepo]
	
		changeWorkingDirectory tmprepo
		git ["fetch", distrepo, "--quiet"]
		git ["reset", "--hard", oldref, "--quiet"]
		git ["merge", newref, "-s", "recursive", "-Xtheirs", "--quiet", "-m", "merging upstream version"]
	
		fetchUpstreamBranch dotpropellor tmprepo
		cleantmprepo
		warnoutofdate dotpropellor True

	getoldrev = takeWhile (/= '\n')
		<$> readProcess "git" ["show-ref", upstreambranch, "--hash"]
	
	git = run "git"
	run cmd ps = unlessM (boolSystem cmd (map Param ps)) $
		error $ "Failed to run " ++ cmd ++ " " ++ show ps

warnoutofdate :: FilePath -> Bool -> IO ()
warnoutofdate dotpropellor havebranch = do
	warningMessage ("** Your " ++ dotpropellor ++ " is out of date..")
	let also s = hPutStrLn stderr ("   " ++ s)
	also ("A newer upstream version is available in " ++ distrepo)
	if havebranch
		then also ("To merge it, run: git merge " ++ upstreambranch)
		else also ("To merge it, find the most recent commit in your repository's history that corresponds to an upstream release of propellor, and set refs/remotes/" ++ upstreambranch ++ " to it. Then run propellor again.")
	also ""

fetchUpstreamBranch :: FilePath -> FilePath -> IO ()
fetchUpstreamBranch dotpropellor repo = do
	changeWorkingDirectory dotpropellor
	void $ boolSystem "git"
		[ Param "fetch"
		, File repo
		, Param ("+refs/heads/master:refs/remotes/" ++ upstreambranch)
		, Param "--quiet"
		]

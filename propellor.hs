-- | Wrapper program for propellor distribution.
--
-- Distributions should install this program into PATH.
-- (Cabal builds it as dict/build/propellor.
--
-- This is not the propellor main program (that's config.hs)
--
-- This installs propellor's source into ~/.propellor,
-- uses it to build the real propellor program (if not already built),
-- and runs it.
-- 
-- The source is either copied from /usr/src/propellor, or is cloned from
-- git over the network.

module Main where

import Utility.UserInfo
import Utility.Monad
import Utility.Process
import Utility.SafeCommand
import Utility.Directory

import Control.Monad
import Control.Monad.IfElse
import System.Directory
import System.FilePath
import System.Environment (getArgs)
import System.Exit
import System.Posix.Directory

srcdir :: FilePath
srcdir = "/usr/src/propellor"

-- Using the github mirror of the main propellor repo because
-- it is accessible over https for better security.
srcrepo :: String
srcrepo = "https://github.com/joeyh/propellor.git"

main :: IO ()
main = do
	args <- getArgs
	home <- myHomeDir
	let propellordir = home </> ".propellor"
	let propellorbin = propellordir </> "propellor"
	wrapper args propellordir propellorbin

wrapper :: [String] -> FilePath -> FilePath -> IO ()
wrapper args propellordir propellorbin = do
	unlessM (doesDirectoryExist propellordir) $
		makeRepo
	buildruncfg
  where
	chain = do
		(_, _, _, pid) <- createProcess (proc propellorbin args) 
		exitWith =<< waitForProcess pid
	makeRepo = do
		putStrLn $ "Setting up your propellor repo in " ++ propellordir
		putStrLn ""
		ifM (doesDirectoryExist srcdir)
			( do
				void $ boolSystem "cp" [Param "-a", File srcdir, File propellordir]
				changeWorkingDirectory propellordir
				void $ boolSystem "git" [Param "init"]
				void $ boolSystem "git" [Param "add", Param "."]
				setuprepo True
			, do
				void $ boolSystem "git" [Param "clone", Param srcrepo, File propellordir] 
				void $ boolSystem "git" [Param "remote", Param "rm", Param "origin"]
				setuprepo False
			)
	setuprepo fromsrcdir = do
		changeWorkingDirectory propellordir
		whenM (doesDirectoryExist "privdata") $
			mapM_ nukeFile =<< dirContents "privdata"
		void $ boolSystem "git" [Param "commit", Param "--allow-empty", Param "--quiet", Param "-m", Param "setting up propellor git repository"]
		void $ boolSystem "git" [Param "remote", Param "add", Param "upstream", Param srcrepo]
		-- Connect synthetic git repo with upstream history so
		-- merging with upstream will work going forward.
		-- Note -s ours is used to avoid getting any divergent
		-- changes from upstream.
		when fromsrcdir $ do
			void $ boolSystem "git" [Param "fetch", Param "upstream"]
			version <- readProcess "dpkg-query" ["--showformat", "${Version}", "--show", "propellor"]
			void $ boolSystem "git" [Param "merge", Param "-s", Param "ours", Param version]
	buildruncfg = do
		changeWorkingDirectory propellordir
		ifM (boolSystem "make" [Param "build"])
			( do
				putStrLn ""
				putStrLn ""
				chain
			, error "Propellor build failed."
			)

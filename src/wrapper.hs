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

localrepo :: FilePath
localrepo = "/usr/src/propellor/propellor.git"

-- Using the github mirror of the main propellor repo because
-- it is accessible over https for better security.
netrepo :: String
netrepo = "https://github.com/joeyh/propellor.git"

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
		localexists <- doesFileExist localrepo <||> doesDirectoryExist localrepo
		void $ boolSystem "git" [Param "clone", File (if localexists then localrepo else netrepo). File propellordir]
	buildruncfg = do
		changeWorkingDirectory propellordir
		ifM (boolSystem "make" [Param "build"])
			( do
				putStrLn ""
				putStrLn ""
				chain
			, error "Propellor build failed."
			)

module Property.GitHome where

import System.FilePath
import System.Directory
import Control.Applicative
import Control.Monad

import Property
import Property.User
import Utility.SafeCommand
import Utility.Directory
import Utility.Monad
import Utility.Exception

{- Clones Joey Hess's git home directory, and runs its fixups script. -}
installedFor :: UserName -> Property
installedFor user = check (not <$> hasGitDir user) $ 
	IOProperty ("githome " ++ user) (go =<< homedir user)
  where
 	go Nothing = noChange
	go (Just home) = do
		let tmpdir = home </> "githome"
		ok <- boolSystem "git" [Param "clone", Param url, Param tmpdir]
			<&&> (and <$> moveout tmpdir home)
			<&&> (catchBoolIO $ removeDirectory tmpdir >> return True)
			<&&> boolSystem "su" [Param "-c", Param "cd; rm -rf .aptitude/ .bashrc .profile; mr checkout; bin/fixups", Param user]
		return $ if ok then MadeChange else FailedChange
	moveout tmpdir home = do
		fs <- dirContents tmpdir
		forM fs $ \f -> boolSystem "mv" [File f, File home]
	url = "git://git.kitenet.net/joey/home"

hasGitDir :: UserName -> IO Bool
hasGitDir user = go =<< homedir user
  where
	go Nothing = return False
	go (Just home) = doesDirectoryExist (home </> ".git")

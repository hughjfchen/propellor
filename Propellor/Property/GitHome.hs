module Propellor.Property.GitHome where

import Propellor.Common
import qualified Propellor.Property.Apt as Apt
import Propellor.Property.User

{- | Clones Joey Hess's git home directory, and runs its fixups script. -}
installedFor :: UserName -> Property
installedFor user = check (not <$> hasGitDir user) $ 
	Property ("githome " ++ user) (go =<< homedir user)
		`requires` Apt.installed ["git", "myrepos"]
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

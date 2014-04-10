module Propellor.Property.SiteSpecific.GitHome where

import Propellor
import qualified Propellor.Property.Apt as Apt
import Propellor.Property.User
import Utility.SafeCommand

-- | Clones Joey Hess's git home directory, and runs its fixups script.
installedFor :: UserName -> Property
installedFor user = check (not <$> hasGitDir user) $ 
	Property ("githome " ++ user) (go =<< homedir user)
		`requires` Apt.installed ["git"]
  where
 	go Nothing = noChange
	go (Just home) = do
		let tmpdir = home </> "githome"
		ensureProperty $ combineProperties "githome setup"
			[ userScriptProperty user ["git clone " ++ url ++ " " ++ tmpdir]
			, Property "moveout" $ makeChange $ void $
				moveout tmpdir home
			, Property "rmdir" $ makeChange $ void $
				catchMaybeIO $ removeDirectory tmpdir
			, userScriptProperty user ["rm -rf .aptitude/ .bashrc .profile; bin/mr checkout; bin/fixups"]
			]
	moveout tmpdir home = do
		fs <- dirContents tmpdir
		forM fs $ \f -> boolSystem "mv" [File f, File home]

url :: String
url = "git://git.kitenet.net/joey/home"

hasGitDir :: UserName -> IO Bool
hasGitDir user = go =<< homedir user
  where
	go Nothing = return False
	go (Just home) = doesDirectoryExist (home </> ".git")

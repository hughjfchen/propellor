module Propellor.Property.SiteSpecific.GitHome where

import Propellor
import qualified Propellor.Property.Apt as Apt
import Propellor.Property.User

-- | Clones Joey Hess's git home directory, and runs its fixups script.
installedFor :: User -> Property NoInfo
installedFor user@(User u) = check (not <$> hasGitDir user) $ 
	property ("githome " ++ u) (go =<< liftIO (homedir user))
		`requires` Apt.installed ["git"]
  where
	go home = do
		let tmpdir = home </> "githome"
		ensureProperty $ combineProperties "githome setup"
			[ userScriptProperty user ["git clone " ++ url ++ " " ++ tmpdir]
			, property "moveout" $ makeChange $ void $
				moveout tmpdir home
			, property "rmdir" $ makeChange $ void $
				catchMaybeIO $ removeDirectory tmpdir
			, userScriptProperty user ["rm -rf .aptitude/ .bashrc .profile; bin/mr checkout; bin/fixups"]
			]
	moveout tmpdir home = do
		fs <- dirContents tmpdir
		forM fs $ \f -> boolSystem "mv" [File f, File home]

url :: String
url = "git://git.kitenet.net/joey/home"

hasGitDir :: User -> IO Bool
hasGitDir user = go =<< homedir user
  where
	go home = doesDirectoryExist (home </> ".git")

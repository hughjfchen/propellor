module Propellor.Keyring where

import Propellor
import Utility.SafeCommand

keyring :: FilePath
keyring = privDataDir </> "keyring.gpg"

addKey :: String -> IO ()
addKey keyid = exitBool =<< allM id [ gpg, gitadd, gitconfig, gitcommit ]
  where
	gpg = do
		createDirectoryIfMissing True privDataDir
		boolSystem "sh"
			[ Param "-c"
			, Param $ "gpg --export " ++ keyid ++ " | gpg " ++
				unwords (gpgopts ++ ["--import"])
			]
	gitadd = boolSystem "git"
		[ Param "add"
		, File keyring
		]

	gitconfig = boolSystem "git"
		[ Param "config"
		, Param "user.signingkey"
		, Param keyid
		]

	gitcommit = gitCommit
		[ File keyring
		, Param "-m"
		, Param "propellor addkey"
		]

	gpgopts =
		[ "--options"
		, "/dev/null"
		, "--no-default-keyring"
		, "--keyring", keyring
		]

{- Automatically sign the commit if there'a a keyring. -}
gitCommit :: [CommandParam] -> IO Bool
gitCommit ps = do
	k <- doesFileExist keyring
	boolSystem "git" $ catMaybes $
		[ Just (Param "commit")
		, if k then Just (Param "--gpg-sign") else Nothing
		] ++ map Just ps

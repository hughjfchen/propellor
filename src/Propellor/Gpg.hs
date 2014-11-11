module Propellor.Gpg where

import Control.Applicative
import System.IO
import System.FilePath
import System.Directory
import Data.Maybe
import Data.List.Utils

import Propellor.PrivData.Paths
import Utility.SafeCommand
import Utility.Process
import Utility.Monad
import Utility.Misc
import Utility.Tmp

type KeyId = String

keyring :: FilePath
keyring = privDataDir </> "keyring.gpg"

listPubKeys :: IO [KeyId]
listPubKeys = parse . lines <$> readProcess "gpg" listopts
  where
	listopts = useKeyringOpts ++ ["--with-colons", "--list-public-keys"]
	parse = mapMaybe (keyIdField . split ":")
	keyIdField ("pub":_:_:_:f:_) = Just f
	keyIdField _ = Nothing

useKeyringOpts :: [String]
useKeyringOpts =
	[ "--options"
	, "/dev/null"
	, "--no-default-keyring"
	, "--keyring", keyring
	]

addKey :: KeyId -> IO ()
addKey keyid = exitBool =<< allM id
	[ gpg, gitadd keyring, reencryptprivdata, gitconfig, gitcommit ]
  where
	gpg = do
		createDirectoryIfMissing True privDataDir
		boolSystem "sh"
			[ Param "-c"
			, Param $ "gpg --export " ++ keyid ++ " | gpg " ++
				unwords (useKeyringOpts ++ ["--import"])
			]

	reencryptprivdata = ifM (doesFileExist privDataFile)
		( do
			gpgEncrypt privDataFile =<< gpgDecrypt privDataFile
			gitadd privDataFile
		, return True
		)

	gitadd f = boolSystem "git"
		[ Param "add"
		, File f
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

{- Automatically sign the commit if there'a a keyring. -}
gitCommit :: [CommandParam] -> IO Bool
gitCommit ps = do
	k <- doesFileExist keyring
	boolSystem "git" $ catMaybes $
		[ Just (Param "commit")
		, if k then Just (Param "--gpg-sign") else Nothing
		] ++ map Just ps

gpgDecrypt :: FilePath -> IO String
gpgDecrypt f = ifM (doesFileExist f)
	( readProcess "gpg" ["--decrypt", f]
	, return ""
	)

gpgEncrypt :: FilePath -> String -> IO ()
gpgEncrypt f s = do
	keyids <- listPubKeys
	let opts =
		[ "--default-recipient-self"
		, "--armor"
		, "--encrypt"
		] ++ concatMap (\k -> ["--recipient", k]) keyids
	encrypted <- writeReadProcessEnv "gpg" opts
		Nothing
		(Just $ flip hPutStr s)
		Nothing
	viaTmp writeFile f encrypted

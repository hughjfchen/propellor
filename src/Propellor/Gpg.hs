module Propellor.Gpg where

import Control.Applicative
import System.IO
import System.FilePath
import System.Directory
import Data.Maybe
import Data.List.Utils

import Propellor.PrivData.Paths
import Propellor.Message
import Utility.SafeCommand
import Utility.Process
import Utility.Monad
import Utility.Misc
import Utility.Tmp

type KeyId = String

keyring :: FilePath
keyring = privDataDir </> "keyring.gpg"

-- Lists the keys in propellor's keyring.
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
addKey keyid = exitBool =<< allM (uncurry actionMessage)
	[ ("adding key to propellor's keyring", addkeyring)
	, ("staging propellor's keyring", gitadd keyring)
	, ("updating encryption of any privdata", reencryptprivdata)
	, ("configuring git signing to use key", gitconfig)
	, ("committing changes", gitcommit)
	]
  where
	addkeyring = do
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

	gitconfig = ifM (snd <$> processTranscript "gpg" ["--list-secret-keys", keyid] Nothing)
		( boolSystem "git"
			[ Param "config"
			, Param "user.signingkey"
			, Param keyid
			]
		, do
			warningMessage $ "Cannot find a secret key for key " ++ keyid ++ ", so not configuring git user.signingkey to use this key."
			return True
		)

	gitcommit = gitCommit
		[ File keyring
		, Param "-m"
		, Param "propellor addkey"
		]

-- Adds --gpg-sign if there's a keyring.
gpgSignParams :: [CommandParam] -> IO [CommandParam]
gpgSignParams ps = ifM (doesFileExist keyring)
	( return (ps ++ [Param "--gpg-sign"])
	, return ps
	)

-- Automatically sign the commit if there'a a keyring.
gitCommit :: [CommandParam] -> IO Bool
gitCommit ps = do
	ps' <- gpgSignParams ps
	boolSystem "git" (Param "commit" : ps')

gpgDecrypt :: FilePath -> IO String
gpgDecrypt f = ifM (doesFileExist f)
	( readProcess "gpg" ["--decrypt", f]
	, return ""
	)

-- Encrypt file to all keys in propellor's keyring.
gpgEncrypt :: FilePath -> String -> IO ()
gpgEncrypt f s = do
	keyids <- listPubKeys
	let opts =
		[ "--default-recipient-self"
		, "--armor"
		, "--encrypt"
		, "--trust-model", "always"
		] ++ concatMap (\k -> ["--recipient", k]) keyids
	encrypted <- writeReadProcessEnv "gpg" opts
		Nothing
		(Just $ flip hPutStr s)
		Nothing
	viaTmp writeFile f encrypted

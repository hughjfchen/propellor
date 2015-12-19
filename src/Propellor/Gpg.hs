module Propellor.Gpg where

import System.IO
import System.FilePath
import System.Directory
import Data.Maybe
import Data.List.Utils
import Control.Monad
import System.Console.Concurrent
import System.Console.Concurrent.Internal (ConcurrentProcessHandle(..))
import Control.Applicative
import Prelude

import Propellor.PrivData.Paths
import Propellor.Message
import Utility.SafeCommand
import Utility.Process
import Utility.Monad
import Utility.Misc
import Utility.Tmp
import Utility.FileSystemEncoding

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
	, ("staging propellor's keyring", gitAdd keyring)
	, ("updating encryption of any privdata", reencryptPrivData)
	, ("configuring git commit signing to use key", gitconfig)
	, ("committing changes", gitCommitKeyRing "add-key")
	]
  where
	addkeyring = do
		createDirectoryIfMissing True privDataDir
		boolSystem "sh"
			[ Param "-c"
			, Param $ "gpg --export " ++ keyid ++ " | gpg " ++
				unwords (useKeyringOpts ++ ["--import"])
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

rmKey :: KeyId -> IO ()
rmKey keyid = exitBool =<< allM (uncurry actionMessage)
	[ ("removing key from propellor's keyring", rmkeyring)
	, ("staging propellor's keyring", gitAdd keyring)
	, ("updating encryption of any privdata", reencryptPrivData)
	, ("configuring git commit signing to not use key", gitconfig)
	, ("committing changes", gitCommitKeyRing "rm-key")
	]
  where
	rmkeyring = boolSystem "gpg" $
		(map Param useKeyringOpts) ++
		[ Param "--batch"
		, Param "--yes"
		, Param "--delete-key", Param keyid
		]

	gitconfig = ifM ((==) (keyid++"\n", True) <$> processTranscript "git" ["config", "user.signingkey"] Nothing)
		( boolSystem "git"
			[ Param "config"
			, Param "--unset"
			, Param "user.signingkey"
			]
		, return True
		)

reencryptPrivData :: IO Bool
reencryptPrivData = ifM (doesFileExist privDataFile)
	( do
		gpgEncrypt privDataFile =<< gpgDecrypt privDataFile
		gitAdd privDataFile
	, return True
	)

gitAdd :: FilePath -> IO Bool
gitAdd f = boolSystem "git"
	[ Param "add"
	, File f
	]

gitCommitKeyRing :: String -> IO Bool
gitCommitKeyRing action = do
	-- Commit explicitly the keyring and privdata files, as other
	-- changes may be staged by the user and shouldn't be committed.
	tocommit <- filterM doesFileExist [ privDataFile, keyring]
	gitCommit (Just ("propellor " ++ action)) (map File tocommit)

-- Adds --gpg-sign if there's a keyring.
gpgSignParams :: [CommandParam] -> IO [CommandParam]
gpgSignParams ps = ifM (doesFileExist keyring)
	( return (ps ++ [Param "--gpg-sign"])
	, return ps
	)

-- Automatically sign the commit if there'a a keyring.
gitCommit :: Maybe String -> [CommandParam] -> IO Bool
gitCommit msg ps = do
	let ps' = Param "commit" : ps ++
		maybe [] (\m -> [Param "-m", Param m]) msg
	ps'' <- gpgSignParams ps'
	if isNothing msg
		then do
			(_, _, _, ConcurrentProcessHandle p) <- createProcessForeground $
				proc "git" (toCommand ps'')
			checkSuccessProcess p
		else boolSystem "git" ps''

gpgDecrypt :: FilePath -> IO String
gpgDecrypt f = ifM (doesFileExist f)
	( writeReadProcessEnv "gpg" ["--decrypt", f] Nothing Nothing (Just fileEncoding)
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
	encrypted <- writeReadProcessEnv "gpg" opts Nothing (Just writer) Nothing
	viaTmp writeFile f encrypted
  where
	writer h = do
		fileEncoding h
		hPutStr h s

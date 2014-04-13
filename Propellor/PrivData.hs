{-# LANGUAGE PackageImports #-}

module Propellor.PrivData where

import qualified Data.Map as M
import Control.Applicative
import System.FilePath
import System.IO
import System.Directory
import Data.Maybe
import Data.List
import Control.Monad
import "mtl" Control.Monad.Reader

import Propellor.Types
import Propellor.Attr
import Propellor.Message
import Utility.Monad
import Utility.PartialPrelude
import Utility.Exception
import Utility.Process
import Utility.Tmp
import Utility.SafeCommand
import Utility.Misc

-- | When the specified PrivDataField is available on the host Propellor
-- is provisioning, it provies the data to the action. Otherwise, it prints
-- a message to help the user make the necessary private data available.
withPrivData :: PrivDataField -> (String -> Propellor Result) -> Propellor Result
withPrivData field a = maybe missing a =<< liftIO (getPrivData field)
  where
	missing = do
		host <- getHostName
		let host' = if ".docker" `isSuffixOf` host
			then "$parent_host"
			else host
		liftIO $ do
			warningMessage $ "Missing privdata " ++ show field
			putStrLn $ "Fix this by running: propellor --set "++host'++" '" ++ show field ++ "'"
			return FailedChange

getPrivData :: PrivDataField -> IO (Maybe String)
getPrivData field = do
	m <- catchDefaultIO Nothing $ readish <$> readFile privDataLocal
	return $ maybe Nothing (M.lookup field) m

setPrivData :: HostName -> PrivDataField -> IO ()
setPrivData host field = do
	putStrLn "Enter private data on stdin; ctrl-D when done:"
	value <- chomp <$> hGetContentsStrict stdin
	makePrivDataDir
	let f = privDataFile host
	m <- fromMaybe M.empty . readish <$> gpgDecrypt f
	let m' = M.insert field value m
	gpgEncrypt f (show m')
	putStrLn "Private data set."
	void $ boolSystem "git" [Param "add", File f]
  where
	chomp s
		| end s == "\n" = chomp (beginning s)
		| otherwise = s

makePrivDataDir :: IO ()
makePrivDataDir = createDirectoryIfMissing False privDataDir

privDataDir :: FilePath
privDataDir = "privdata"

privDataFile :: HostName -> FilePath
privDataFile host = privDataDir </> host ++ ".gpg"

privDataLocal :: FilePath
privDataLocal = privDataDir </> "local"

gpgDecrypt :: FilePath -> IO String
gpgDecrypt f = ifM (doesFileExist f)
	( readProcess "gpg" ["--decrypt", f]
	, return ""
	)

gpgEncrypt :: FilePath -> String -> IO ()
gpgEncrypt f s = do
	encrypted <- writeReadProcessEnv "gpg"
		[ "--default-recipient-self"
		, "--armor"
		, "--encrypt"
		]
		Nothing
		(Just $ flip hPutStr s)
		Nothing
	viaTmp writeFile f encrypted

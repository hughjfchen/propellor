{-# LANGUAGE PackageImports #-}

module Propellor.PrivData where

import Control.Applicative
import System.IO
import System.Directory
import Data.Maybe
import Data.Monoid
import Data.List
import Control.Monad
import Control.Monad.IfElse
import "mtl" Control.Monad.Reader
import qualified Data.Map as M
import qualified Data.Set as S

import Propellor.Types
import Propellor.Types.Info
import Propellor.Message
import Propellor.Info
import Propellor.Gpg
import Propellor.PrivData.Paths
import Utility.Monad
import Utility.PartialPrelude
import Utility.Exception
import Utility.Tmp
import Utility.SafeCommand
import Utility.Misc
import Utility.FileMode
import Utility.Env
import Utility.Table

-- | Allows a Property to access the value of a specific PrivDataField,
-- for use in a specific Context.
--
-- Example use:
--
-- > withPrivData (PrivFile pemfile) (Context "joeyh.name") $ \getdata ->
-- >     property "joeyh.name ssl cert" $ getdata $ \privdata ->
-- >       liftIO $ writeFile pemfile privdata
-- >   where pemfile = "/etc/ssl/certs/web.pem"
-- 
-- Note that if the value is not available, the action is not run
-- and instead it prints a message to help the user make the necessary
-- private data available.
--
-- The resulting Property includes Info about the PrivDataField
-- being used, which is necessary to ensure that the privdata is sent to
-- the remote host by propellor.
withPrivData
	:: PrivDataField
	-> Context
	-> (((PrivData -> Propellor Result) -> Propellor Result) -> Property)
	-> Property
withPrivData field context@(Context cname) mkprop = addinfo $ mkprop $ \a ->
	maybe missing a =<< liftIO (getLocalPrivData field context)
  where
	missing = liftIO $ do
		warningMessage $ "Missing privdata " ++ show field ++ " (for " ++ cname ++ ")"
		putStrLn $ "Fix this by running: propellor --set '" ++ show field ++ "' '" ++ cname ++ "'"
		return FailedChange
	addinfo p = p { propertyInfo = propertyInfo p <> mempty { _privDataFields = S.singleton (field, context) } }

addPrivDataField :: (PrivDataField, Context) -> Property
addPrivDataField v = pureInfoProperty (show v) $
	mempty { _privDataFields = S.singleton v }

{- Gets the requested field's value, in the specified context if it's
 - available, from the host's local privdata cache. -}
getLocalPrivData :: PrivDataField -> Context -> IO (Maybe PrivData)
getLocalPrivData field context =
	getPrivData field context . fromMaybe M.empty <$> localcache
  where
	localcache = catchDefaultIO Nothing $ readish <$> readFile privDataLocal

type PrivMap = M.Map (PrivDataField, Context) PrivData

{- Get only the set of PrivData that the Host's Info says it uses. -}
filterPrivData :: Host -> PrivMap -> PrivMap
filterPrivData host = M.filterWithKey (\k _v -> S.member k used)
  where
	used = _privDataFields $ hostInfo host

getPrivData :: PrivDataField -> Context -> PrivMap -> Maybe PrivData
getPrivData field context = M.lookup (field, context)

setPrivData :: PrivDataField -> Context -> IO ()
setPrivData field context = do
	putStrLn "Enter private data on stdin; ctrl-D when done:"
	setPrivDataTo field context =<< hGetContentsStrict stdin

dumpPrivData :: PrivDataField -> Context -> IO ()
dumpPrivData field context =
	maybe (error "Requested privdata is not set.") putStrLn
		=<< (getPrivData field context <$> decryptPrivData)

editPrivData :: PrivDataField -> Context -> IO ()
editPrivData field context = do
	v <- getPrivData field context <$> decryptPrivData
	v' <- withTmpFile "propellorXXXX" $ \f h -> do
		hClose h
		maybe noop (writeFileProtected f) v
		editor <- getEnvDefault "EDITOR" "vi"
		unlessM (boolSystem editor [File f]) $
			error "Editor failed; aborting."
		readFile f
	setPrivDataTo field context v'

listPrivDataFields :: [Host] -> IO ()
listPrivDataFields hosts = do
	m <- decryptPrivData
	showtable "Currently set data:" $
		map mkrow (M.keys m)
	showtable "Data that would be used if set:" $
		map mkrow (M.keys $ M.difference wantedmap m)
  where
	header = ["Field", "Context", "Used by"]
	mkrow k@(field, (Context context)) =
		[ shellEscape $ show field
		, shellEscape context
		, intercalate ", " $ sort $ fromMaybe [] $ M.lookup k usedby
		]
	mkhostmap host = M.fromList $ map (\k -> (k, [hostName host])) $
		S.toList $ _privDataFields $ hostInfo host
	usedby = M.unionsWith (++) $ map mkhostmap hosts
	wantedmap = M.fromList $ zip (M.keys usedby) (repeat "")
	showtable desc rows = do
		putStrLn $ "\n" ++ desc
		putStr $ unlines $ formatTable $ tableWithHeader header rows

setPrivDataTo :: PrivDataField -> Context -> PrivData -> IO ()
setPrivDataTo field context value = do
	makePrivDataDir
	m <- decryptPrivData
	let m' = M.insert (field, context) (chomp value) m
	gpgEncrypt privDataFile (show m')
	putStrLn "Private data set."
	void $ boolSystem "git" [Param "add", File privDataFile]
  where
	chomp s
		| end s == "\n" = chomp (beginning s)
		| otherwise = s

decryptPrivData :: IO PrivMap
decryptPrivData = fromMaybe M.empty . readish <$> gpgDecrypt privDataFile

makePrivDataDir :: IO ()
makePrivDataDir = createDirectoryIfMissing False privDataDir

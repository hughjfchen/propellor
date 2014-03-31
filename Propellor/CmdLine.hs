module Propellor.CmdLine where

import System.Environment
import Data.List
import System.Exit
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Base64.Lazy as B64
import Data.Bits.Utils

import Propellor
import Utility.FileMode
import Utility.SafeCommand
import Utility.Data

data CmdLine
	= Run HostName
	| Spin HostName
	| Boot HostName
	| Set HostName PrivDataField
	| AddKey String

processCmdLine :: IO CmdLine
processCmdLine = go =<< getArgs
  where
  	go ("--help":_) = usage
  	go ("--spin":h:[]) = return $ Spin h
  	go ("--boot":h:[]) = return $ Boot h
	go ("--add-key":k:[]) = return $ AddKey k
	go ("--set":h:f:[]) = case readish f of
		Just pf -> return $ Set h pf
		Nothing -> error $ "Unknown privdata field " ++ f
	go (h:[]) = return $ Run h
	go [] = do
		s <- takeWhile (/= '\n') <$> readProcess "hostname" ["-f"]
		if null s
			then error "Cannot determine hostname! Pass it on the command line."
			else return $ Run s
	go _ = usage
	
usage :: IO a
usage = do
	putStrLn $ unlines 
		[ "Usage:"
		, "  propellor"
		, "  propellor hostname"
		, "  propellor --spin hostname"
		, "  propellor --set hostname field"
		, "  propellor --add-key keyid"
		]
	exitFailure

defaultMain :: (HostName -> Maybe [Property]) -> IO ()
defaultMain getprops = go =<< processCmdLine
  where
	go (Run host) = maybe (unknownhost host) ensureProperties (getprops host)
	go (Spin host) = spin host
	go (Boot host) = maybe (unknownhost host) boot (getprops host)
	go (Set host field) = setPrivData host field
	go (AddKey keyid) = addKey keyid

unknownhost :: HostName -> IO a
unknownhost h = error $ unwords
	[ "Unknown host:", h
	, "(perhaps you should specify the real hostname on the command line?)"
	]

spin :: HostName -> IO ()
spin host = do
	url <- getUrl
	void $ gitCommit [Param "--allow-empty", Param "-a", Param "-m", Param "propellor spin"]
	void $ boolSystem "git" [Param "push"]
	privdata <- gpgDecrypt (privDataFile host)
	withBothHandles createProcessSuccess (proc "ssh" [user, bootstrapcmd url]) $ \(toh, fromh) -> do
		status <- getstatus fromh `catchIO` error "protocol error"
		case status of
			NeedKeyRing -> do
				putStr $ "Sending " ++ keyring ++ " to " ++ host ++ "..."
				hFlush stdout
				s <- w82s . BL.unpack . B64.encode
					<$> BL.readFile keyring
				hPutStrLn toh $ toMarked keyringMarker s
				hFlush toh
				putStrLn "done"
			HaveKeyRing -> noop
		hPutStrLn toh $ toMarked privDataMarker privdata
		hFlush toh
		hClose toh

		-- Display remaining output.
		void $ tryIO $ forever $
			showremote =<< hGetLine fromh
		hClose fromh

  where
	user = "root@"++host
	bootstrapcmd url = shellWrap $ intercalate " && "
		[ intercalate " ; "
			[ "if [ ! -d " ++ localdir ++ " ]"
			, "then " ++ intercalate " && "
				[ "apt-get -y install git"
				, "git clone " ++ url ++ " " ++ localdir
				]
			, "fi"
			]
		, "cd " ++ localdir
		, "make pull build"
		, "./propellor --boot " ++ host
		]
	getstatus :: Handle -> IO BootStrapStatus
	getstatus h = do
		l <- hGetLine h
		case readish =<< fromMarked statusMarker l of
			Nothing -> do
				showremote l
				getstatus h
			Just status -> return status
	showremote s = putStrLn s

data BootStrapStatus = HaveKeyRing | NeedKeyRing
	deriving (Read, Show, Eq)

type Marker = String
type Marked = String

statusMarker :: Marker
statusMarker = "STATUS"

keyringMarker :: Marker
keyringMarker = "KEYRING"

privDataMarker :: String
privDataMarker = "PRIVDATA "

toMarked :: Marker -> String -> String
toMarked marker = unlines . map (marker ++) . lines

fromMarked :: Marker -> Marked -> Maybe String
fromMarked marker s
	| null matches = Nothing
	| otherwise = Just $ unlines $ map (drop len) matches
  where
	len = length marker
	matches = filter (marker `isPrefixOf`) $ lines s

boot :: [Property] -> IO ()
boot props = do
	havering <- doesFileExist keyring
	putStrLn $ toMarked statusMarker $ show $ if havering then HaveKeyRing else NeedKeyRing
	hFlush stdout
	reply <- hGetContentsStrict stdin

	hPutStrLn stderr $ show $ B64.decode . BL.pack . s2w8 <$> fromMarked keyringMarker reply
	hFlush stderr

	makePrivDataDir
	maybe noop (writeFileProtected privDataLocal) $
		fromMarked privDataMarker reply
	case eitherToMaybe . B64.decode . BL.pack . s2w8 =<< fromMarked keyringMarker reply of
		Nothing -> noop
		Just d -> do
			writeFileProtected keyring ""
			BL.writeFile keyring d
	ensureProperties props

addKey :: String -> IO ()
addKey keyid = exitBool =<< allM id [ gpg, gitadd, gitcommit ]
  where
	gpg = boolSystem "sh"
		[ Param "-c"
		, Param $ "gpg --export " ++ keyid ++ " | gpg " ++
			unwords (gpgopts ++ ["--import"])
		]
	gitadd = boolSystem "git"
		[ Param "add"
		, File keyring
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

keyring :: FilePath
keyring = privDataDir </> "keyring.gpg"

gpgopts :: [String]
gpgopts = ["--options", "/dev/null", "--no-default-keyring", "--keyring", keyring]

localdir :: FilePath
localdir = "/usr/local/propellor"

getUrl :: IO String
getUrl = fromMaybe nourl <$> getM get urls
  where
	urls = ["remote.deploy.url", "remote.origin.url"]
	nourl = error $ "Cannot find deploy url in " ++ show urls
	get u = do
		v <- catchMaybeIO $ 
			takeWhile (/= '\n') 
				<$> readProcess "git" ["config", u]
		return $ case v of
			Just url | not (null url) -> Just url
			_ -> Nothing

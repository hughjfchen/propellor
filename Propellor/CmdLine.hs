module Propellor.CmdLine where

import System.Environment
import Data.List
import System.Exit

import Propellor
import Utility.FileMode
import Utility.SafeCommand

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
	go (Run host) = withprops host $ pullFirst . ensureProperties
	go (Spin host) = withprops host $ const $ spin host
	go (Boot host) = withprops host $ pullFirst . boot
	go (Set host field) = setPrivData host field
	go (AddKey keyid) = addKey keyid
	withprops host a = maybe (unknownhost host) a (getprops host)

unknownhost :: HostName -> IO a
unknownhost h = error $ unwords
	[ "Unknown host:", h
	, "(perhaps you should specify the real hostname on the command line?)"
	]

pullFirst :: IO () -> IO ()
pullFirst next = do
	branchref <- takeWhile (/= '\n') 
		<$> readProcess "git" ["symbolic-ref", "HEAD"]
	let originbranch = "origin" </> takeFileName branchref
	void $ boolSystem "git" [Param "fetch"]
	
	whenM (doesFileExist keyring) $ do
		{- To verify origin/master commit's signature, have to
		 - convince gpg to use our keyring. While running git log.
		 - Which has no way to pass options to gpg.
		 - Argh! -}
		let gpgconf = privDataDir </> "gpg.conf"
		writeFile gpgconf $ unlines
			[ " keyring " ++ keyring
			, "no-auto-check-trustdb"
			]
		-- gpg is picky about perms
		modifyFileMode privDataDir (removeModes otherGroupModes)
		s <- readProcessEnv "git" ["log", "-n", "1", "--format=%G?", originbranch]
			(Just [("GNUPGHOME", privDataDir)])
		nukeFile $ privDataDir </> "trustring.gpg"
		nukeFile $ privDataDir </> "gpg.conf"
		if s /= "U\n" && s/= "G\n"
			then error $ "git branch " ++ originbranch ++ " is not signed with a trusted gpg key; refusing to deploy it!"
			else putStrLn "git branch " ++ originbranch ++ " gpg signature verified; merging"
	
	void $ boolSystem "git" [Param "merge", Param originbranch]

	next

spin :: HostName -> IO ()
spin host = do
	url <- getUrl
	void $ gitCommit [Param "--allow-empty", Param "-a", Param "-m", Param "propellor spin"]
	void $ boolSystem "git" [Param "push"]
	go url =<< gpgDecrypt (privDataFile host)
  where
	go url privdata = withBothHandles createProcessSuccess (proc "ssh" [user, bootstrapcmd]) $ \(toh, fromh) -> do
		let finish = do
			senddata toh (privDataFile host) privDataMarker privdata
			hClose toh
			
			-- Display remaining output.
			void $ tryIO $ forever $
				showremote =<< hGetLine fromh
			hClose fromh
		status <- getstatus fromh `catchIO` error "protocol error"
		case status of
			Ready -> finish
			NeedGitClone -> do
				hClose toh
				hClose fromh
				sendGitClone host url
				go url privdata
	
	user = "root@"++host

	bootstrapcmd = shellWrap $ intercalate " && "
		[ intercalate " ; "
			[ "if [ ! -d " ++ localdir ++ " ]"
			, "then " ++ intercalate " && "
				[ "apt-get -y install git"
				, "echo " ++ toMarked statusMarker (show NeedGitClone)
				]
			, "fi"
			]
		, "cd " ++ localdir
		, "make build"
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
	senddata toh f marker s = do
		putStr $ "Sending " ++ f ++ " (" ++ show (length s) ++ " bytes) to " ++ host ++ "..."
		hFlush stdout
		hPutStrLn toh $ toMarked marker s
		hFlush toh
		putStrLn "done"

sendGitClone :: HostName -> String -> IO ()
sendGitClone host url = do
	putStrLn $ "Pushing git repository to " ++ host
	withTmpFile "gitbundle" $ \tmp _ -> do
		-- TODO: ssh connection caching, or better push method
		-- with less connections.
		void $ boolSystem "git" [Param "bundle", Param "create", File tmp, Param "HEAD"]
		void $ boolSystem "scp" [File tmp, Param ("root@"++host++":"++remotebundle)]
		void $ boolSystem "ssh" [Param ("root@"++host), Param unpackcmd]
  where
	remotebundle = "/usr/local/propellor.git"
	unpackcmd = shellWrap $ intercalate " && "
		[ "git clone " ++ remotebundle ++ " " ++ localdir
		, "cd " ++ localdir
		, "git checkout -b master"
		, "git remote rm origin"
		, "git remote add origin " ++ url
		, "rm -f " ++ remotebundle
		]

data BootStrapStatus = Ready | NeedGitClone
	deriving (Read, Show, Eq)

type Marker = String
type Marked = String

statusMarker :: Marker
statusMarker = "STATUS"

privDataMarker :: String
privDataMarker = "PRIVDATA "

toMarked :: Marker -> String -> String
toMarked marker = intercalate "\n" . map (marker ++) . lines

fromMarked :: Marker -> Marked -> Maybe String
fromMarked marker s
	| null matches = Nothing
	| otherwise = Just $ intercalate "\n" $
		map (drop len) matches
  where
	len = length marker
	matches = filter (marker `isPrefixOf`) $ lines s

boot :: [Property] -> IO ()
boot props = do
	putStrLn $ toMarked statusMarker $ show Ready
	hFlush stdout
	reply <- hGetContentsStrict stdin

	makePrivDataDir
	maybe noop (writeFileProtected privDataLocal) $
		fromMarked privDataMarker reply
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

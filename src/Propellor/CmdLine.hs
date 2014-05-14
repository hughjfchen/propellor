module Propellor.CmdLine where

import System.Environment (getArgs)
import Data.List
import System.Exit
import System.Log.Logger
import System.Log.Formatter
import System.Log.Handler (setFormatter, LogHandler)
import System.Log.Handler.Simple
import System.PosixCompat
import Control.Exception (bracket)
import System.Posix.IO
import Data.Time.Clock.POSIX

import Propellor
import qualified Propellor.Property.Docker as Docker
import qualified Propellor.Property.Docker.Shim as DockerShim
import Utility.FileMode
import Utility.SafeCommand
import Utility.UserInfo

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

processCmdLine :: IO CmdLine
processCmdLine = go =<< getArgs
  where
  	go ("--help":_) = usage
  	go ("--spin":h:[]) = return $ Spin h
  	go ("--boot":h:[]) = return $ Boot h
	go ("--add-key":k:[]) = return $ AddKey k
	go ("--set":h:f:[]) = case readish f of
		Just pf -> return $ Set h pf
		Nothing -> errorMessage $ "Unknown privdata field " ++ f
	go ("--continue":s:[]) = case readish s of
		Just cmdline -> return $ Continue cmdline
		Nothing -> errorMessage "--continue serialization failure"
  	go ("--chain":h:[]) = return $ Chain h
	go ("--docker":h:[]) = return $ Docker h
	go (h:[])
		| "--" `isPrefixOf` h = usage
		| otherwise = return $ Run h
	go [] = do
		s <- takeWhile (/= '\n') <$> readProcess "hostname" ["-f"]
		if null s
			then errorMessage "Cannot determine hostname! Pass it on the command line."
			else return $ Run s
	go _ = usage

defaultMain :: [Host] -> IO ()
defaultMain hostlist = do
	DockerShim.cleanEnv
	checkDebugMode
	cmdline <- processCmdLine
	debug ["command line: ", show cmdline]
	go True cmdline
  where
	go _ (Continue cmdline) = go False cmdline
	go _ (Set hn field) = setPrivData hn field
	go _ (AddKey keyid) = addKey keyid
	go _ (Chain hn) = withprops hn $ \attr ps -> do
		r <- runPropellor attr $ ensureProperties ps
		putStrLn $ "\n" ++ show r
	go _ (Docker hn) = Docker.chain hn
	go True cmdline@(Spin _) = buildFirst cmdline $ go False cmdline
	go True cmdline = updateFirst cmdline $ go False cmdline
	go False (Spin hn) = withprops hn $ const . const $ spin hn
	go False (Run hn) = ifM ((==) 0 <$> getRealUserID)
		( onlyProcess $ withprops hn mainProperties
		, go True (Spin hn)
		)
	go False (Boot hn) = onlyProcess $ withprops hn boot

	withprops :: HostName -> (Attr -> [Property] -> IO ()) -> IO ()
	withprops hn a = maybe
		(unknownhost hn)
		(\h -> a (hostAttr h) (hostProperties h))
		(findHost hostlist hn)

onlyProcess :: IO a -> IO a
onlyProcess a = bracket lock unlock (const a)
  where
	lock = do
		l <- createFile lockfile stdFileMode
		setLock l (WriteLock, AbsoluteSeek, 0, 0)
			`catchIO` const alreadyrunning
		return l
	unlock = closeFd
	alreadyrunning = error "Propellor is already running on this host!"
	lockfile = localdir </> ".lock"

unknownhost :: HostName -> IO a
unknownhost h = errorMessage $ unlines
	[ "Propellor does not know about host: " ++ h
	, "(Perhaps you should specify the real hostname on the command line?)"
	, "(Or, edit propellor's config.hs to configure this host)"
	]

buildFirst :: CmdLine -> IO () -> IO ()
buildFirst cmdline next = do
	oldtime <- getmtime
	ifM (actionMessage "Propellor build" $ boolSystem "make" [Param "build"])
		( do
			newtime <- getmtime
			if newtime == oldtime
				then next
				else void $ boolSystem "./propellor" [Param "--continue", Param (show cmdline)]
		, errorMessage "Propellor build failed!" 
		)
  where
	getmtime = catchMaybeIO $ getModificationTime "propellor"

getCurrentBranch :: IO String
getCurrentBranch = takeWhile (/= '\n') 
	<$> readProcess "git" ["symbolic-ref", "--short", "HEAD"]

updateFirst :: CmdLine -> IO () -> IO ()
updateFirst cmdline next = do
	branchref <- getCurrentBranch
	let originbranch = "origin" </> branchref

	void $ actionMessage "Git fetch" $ boolSystem "git" [Param "fetch"]
	
	whenM (doesFileExist keyring) $ do
		{- To verify origin branch commit's signature, have to
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
		nukeFile $ privDataDir </> "trustdb.gpg"
		nukeFile $ privDataDir </> "pubring.gpg"
		nukeFile $ privDataDir </> "gpg.conf"
		if s == "U\n" || s == "G\n"
			then do
				putStrLn $ "git branch " ++ originbranch ++ " gpg signature verified; merging"
				hFlush stdout
			else errorMessage $ "git branch " ++ originbranch ++ " is not signed with a trusted gpg key; refusing to deploy it!"
	
	oldsha <- getCurrentGitSha1 branchref
	void $ boolSystem "git" [Param "merge", Param originbranch]
	newsha <- getCurrentGitSha1 branchref

	if oldsha == newsha
		then next
		else ifM (actionMessage "Propellor build" $ boolSystem "make" [Param "build"])
			( void $ boolSystem "./propellor" [Param "--continue", Param (show cmdline)]
			, errorMessage "Propellor build failed!" 
			)

getCurrentGitSha1 :: String -> IO String
getCurrentGitSha1 branchref = readProcess "git" ["show-ref", "--hash", branchref]

spin :: HostName -> IO ()
spin hn = do
	url <- getUrl
	void $ gitCommit [Param "--allow-empty", Param "-a", Param "-m", Param "propellor spin"]
	void $ boolSystem "git" [Param "push"]
	cacheparams <- toCommand <$> sshCachingParams hn
	go cacheparams url =<< gpgDecrypt (privDataFile hn)
  where
	go cacheparams url privdata = withBothHandles createProcessSuccess (proc "ssh" $ cacheparams ++ [user, bootstrapcmd]) $ \(toh, fromh) -> do
		let finish = do
			senddata toh (privDataFile hn) privDataMarker privdata
			hClose toh
			
			-- Display remaining output.
			void $ tryIO $ forever $
				showremote =<< hGetLine fromh
			hClose fromh
		status <- getstatus fromh `catchIO` (const $ errorMessage "protocol error (perhaps the remote propellor failed to run?)")
		case status of
			Ready -> finish
			NeedGitClone -> do
				hClose toh
				hClose fromh
				sendGitClone hn url
				go cacheparams url privdata
	
	user = "root@"++hn

	bootstrapcmd = shellWrap $ intercalate " ; "
		[ "if [ ! -d " ++ localdir ++ " ]"
		, "then " ++ intercalate " && "
			[ "apt-get --no-install-recommends --no-upgrade -y install git make"
			, "echo " ++ toMarked statusMarker (show NeedGitClone)
			]
		, "else " ++ intercalate " && "
			[ "cd " ++ localdir
			, "if ! test -x ./propellor; then make deps build; fi"
			, "./propellor --boot " ++ hn
			]
		, "fi"
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
	senddata toh f marker s = void $
		actionMessage ("Sending " ++ f ++ " (" ++ show (length s) ++ " bytes) to " ++ hn) $ do
			sendMarked toh marker s
			return True

sendGitClone :: HostName -> String -> IO ()
sendGitClone hn url = void $ actionMessage ("Pushing git repository to " ++ hn) $ do
	branch <- getCurrentBranch
	cacheparams <- sshCachingParams hn
	withTmpFile "propellor.git" $ \tmp _ -> allM id
		[ boolSystem "git" [Param "bundle", Param "create", File tmp, Param "HEAD"]
		, boolSystem "scp" $ cacheparams ++ [File tmp, Param ("root@"++hn++":"++remotebundle)]
		, boolSystem "ssh" $ cacheparams ++ [Param ("root@"++hn), Param $ unpackcmd branch]
		]
  where
	remotebundle = "/usr/local/propellor.git"
	unpackcmd branch = shellWrap $ intercalate " && "
		[ "git clone " ++ remotebundle ++ " " ++ localdir
		, "cd " ++ localdir
		, "git checkout -b " ++ branch
		, "git remote rm origin"
		, "rm -f " ++ remotebundle
		, "git remote add origin " ++ url
		-- same as --set-upstream-to, except origin branch
		-- has not been pulled yet
		, "git config branch."++branch++".remote origin"
		, "git config branch."++branch++".merge refs/heads/"++branch
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

sendMarked :: Handle -> Marker -> String -> IO ()
sendMarked h marker s = do
	-- Prefix string with newline because sometimes a
	-- incomplete line is output.
	hPutStrLn h ("\n" ++ toMarked marker s)
	hFlush h

fromMarked :: Marker -> Marked -> Maybe String
fromMarked marker s
	| null matches = Nothing
	| otherwise = Just $ intercalate "\n" $
		map (drop len) matches
  where
	len = length marker
	matches = filter (marker `isPrefixOf`) $ lines s

boot :: Attr -> [Property] -> IO ()
boot attr ps = do
	sendMarked stdout statusMarker $ show Ready
	reply <- hGetContentsStrict stdin

	makePrivDataDir
	maybe noop (writeFileProtected privDataLocal) $
		fromMarked privDataMarker reply
	mainProperties attr ps

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

getUrl :: IO String
getUrl = maybe nourl return =<< getM get urls
  where
	urls = ["remote.deploy.url", "remote.origin.url"]
	nourl = errorMessage $ "Cannot find deploy url in " ++ show urls
	get u = do
		v <- catchMaybeIO $ 
			takeWhile (/= '\n') 
				<$> readProcess "git" ["config", u]
		return $ case v of
			Just url | not (null url) -> Just url
			_ -> Nothing

checkDebugMode :: IO ()
checkDebugMode = go =<< getEnv "PROPELLOR_DEBUG"
  where
	go (Just s)
		| s == "1" = do
		f <- setFormatter
			<$> streamHandler stderr DEBUG
			<*> pure (simpleLogFormatter "[$time] $msg")
		updateGlobalLogger rootLoggerName $ 
			setLevel DEBUG .  setHandlers [f]
	go _ = noop

-- Parameters can be passed to both ssh and scp, to enable a ssh connection
-- caching socket.
--
-- If the socket already exists, check if its mtime is older than 10
-- minutes, and if so stop that ssh process, in order to not try to
-- use an old stale connection. (atime would be nicer, but there's
-- a good chance a laptop uses noatime)
sshCachingParams :: HostName -> IO [CommandParam]
sshCachingParams hn = do
	home <- myHomeDir
	let cachedir = home </> ".ssh" </> "propellor"
	createDirectoryIfMissing False cachedir
	let socketfile = cachedir </> hn ++ ".sock"
	let ps = 
		[ Param "-o", Param ("ControlPath=" ++ socketfile)
		, Params "-o ControlMaster=auto -o ControlPersist=yes"
		]

	maybe noop (expireold ps socketfile)
		=<< catchMaybeIO (getFileStatus socketfile)
	
	return ps
		
  where
	expireold ps f s = do
		now <- truncate <$> getPOSIXTime :: IO Integer
		if modificationTime s > fromIntegral now - tenminutes
			then touchFile f
			else do
				void $ boolSystem "ssh" $
					[ Params "-O stop" ] ++ ps ++
					[ Param "localhost" ]
				nukeFile f
	tenminutes = 600

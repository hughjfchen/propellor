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
import Control.Concurrent.Async
import qualified Data.ByteString as B
import System.Process (std_in, std_out)

import Propellor
import Propellor.Protocol
import Propellor.PrivData.Paths
import Propellor.Gpg
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
		, "  propellor --add-key keyid"
		, "  propellor --set field context"
		, "  propellor --dump field context"
		, "  propellor --edit field context"
		, "  propellor --list-fields"
		]
	exitFailure

processCmdLine :: IO CmdLine
processCmdLine = go =<< getArgs
  where
	go ("--help":_) = usage
	go ("--spin":h:[]) = return $ Spin h
	go ("--boot":h:[]) = return $ Boot h
	go ("--run":h:[]) = return $ Run h
	go ("--add-key":k:[]) = return $ AddKey k
	go ("--set":f:c:[]) = withprivfield f c Set
	go ("--dump":f:c:[]) = withprivfield f c Dump
	go ("--edit":f:c:[]) = withprivfield f c Edit
	go ("--list-fields":[]) = return ListFields
	go ("--continue":s:[]) = case readish s of
		Just cmdline -> return $ Continue cmdline
		Nothing -> errorMessage "--continue serialization failure"
	go ("--chain":h:[]) = return $ Chain h
	go ("--docker":h:[]) = return $ Docker h
	go ("--gitpush":fin:fout:_) = return $ GitPush (Prelude.read fin) (Prelude.read fout)
	go (h:[])
		| "--" `isPrefixOf` h = usage
		| otherwise = return $ Run h
	go [] = do
		s <- takeWhile (/= '\n') <$> readProcess "hostname" ["-f"]
		if null s
			then errorMessage "Cannot determine hostname! Pass it on the command line."
			else return $ Run s
	go _ = usage

	withprivfield s c f = case readish s of
		Just pf -> return $ f pf (Context c)
		Nothing -> errorMessage $ "Unknown privdata field " ++ s

defaultMain :: [Host] -> IO ()
defaultMain hostlist = do
	DockerShim.cleanEnv
	checkDebugMode
	cmdline <- processCmdLine
	debug ["command line: ", show cmdline]
	go True cmdline
  where
	go _ (Continue cmdline) = go False cmdline
	go _ (Set field context) = setPrivData field context
	go _ (Dump field context) = dumpPrivData field context
	go _ (Edit field context) = editPrivData field context
	go _ ListFields = listPrivDataFields hostlist
	go _ (AddKey keyid) = addKey keyid
	go _ (Chain hn) = withhost hn $ \h -> do
		r <- runPropellor h $ ensureProperties $ hostProperties h
		putStrLn $ "\n" ++ show r
	go _ (Docker hn) = Docker.chain hn
	go _ (GitPush fin fout) = gitPush fin fout
	go True cmdline@(Spin _) = buildFirst cmdline $ go False cmdline
	go True cmdline = updateFirst cmdline $ go False cmdline
	go False (Spin hn) = withhost hn $ spin hn
	go False (Run hn) = ifM ((==) 0 <$> getRealUserID)
		( onlyProcess $ withhost hn mainProperties
		, go True (Spin hn)
		)
	go False (Boot _) = onlyProcess boot

	withhost :: HostName -> (Host -> IO ()) -> IO ()
	withhost hn a = maybe (unknownhost hn hostlist) a (findHost hostlist hn)

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

unknownhost :: HostName -> [Host] -> IO a
unknownhost h hosts = errorMessage $ unlines
	[ "Propellor does not know about host: " ++ h
	, "(Perhaps you should specify the real hostname on the command line?)"
	, "(Or, edit propellor's config.hs to configure this host)"
	, "Known hosts: " ++ unwords (map hostName hosts)
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
updateFirst cmdline next = ifM hasOrigin (updateFirst' cmdline next, next)

updateFirst' :: CmdLine -> IO () -> IO ()
updateFirst' cmdline next = do
	branchref <- getCurrentBranch
	let originbranch = "origin" </> branchref

	void $ actionMessage "Git fetch" $ boolSystem "git" [Param "fetch"]
	
	oldsha <- getCurrentGitSha1 branchref
	
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
				void $ boolSystem "git" [Param "merge", Param originbranch]
			else warningMessage $ "git branch " ++ originbranch ++ " is not signed with a trusted gpg key; refusing to deploy it! (Running with previous configuration instead.)"
	
	newsha <- getCurrentGitSha1 branchref

	if oldsha == newsha
		then next
		else ifM (actionMessage "Propellor build" $ boolSystem "make" [Param "build"])
			( void $ boolSystem "./propellor" [Param "--continue", Param (show cmdline)]
			, errorMessage "Propellor build failed!" 
			)

getCurrentGitSha1 :: String -> IO String
getCurrentGitSha1 branchref = readProcess "git" ["show-ref", "--hash", branchref]

-- spin handles deploying propellor to a remote host, if it's not already
-- installed there, or updating it if it is. Once the remote propellor is
-- updated, it's run.
spin :: HostName -> Host -> IO ()
spin hn hst = do
	void $ gitCommit [Param "--allow-empty", Param "-a", Param "-m", Param "propellor spin"]
	void $ boolSystem "git" [Param "push"]
	cacheparams <- toCommand <$> sshCachingParams hn
	go cacheparams =<< hostprivdata
	unlessM (boolSystem "ssh" (map Param (cacheparams ++ ["-t", user, runcmd]))) $
		error $ "remote propellor failed (running: " ++ runcmd ++")"
  where
	hostprivdata = show . filterPrivData hst <$> decryptPrivData

	go cacheparams privdata = withBothHandles createProcessSuccess (proc "ssh" $ cacheparams ++ [user, bootstrapcmd]) $ \(toh, fromh) -> do
		let loop = do
			status <- getMarked fromh statusMarker
			case readish =<< status of
				Just NeedRepoUrl -> do
					sendMarked toh repoUrlMarker
						=<< (fromMaybe "" <$> getRepoUrl)
					loop
				Just NeedPrivData -> do
					sendprivdata toh privdata
					loop
				Just NeedGitPush -> do
					sendMarked toh gitPushMarker ""
					let p = (proc "git" ["upload-pack", "."])
						{ std_in = UseHandle fromh
						, std_out = UseHandle toh
						}
					(Nothing, Nothing, Nothing, h) <- createProcess p
					unlessM ((==) ExitSuccess <$> waitForProcess h) $
						errorMessage "git upload-pack failed"
					-- no more protocol possible after
					-- git push
				Just NeedGitClone -> do
					hClose toh
					hClose fromh
					sendGitClone hn
					go cacheparams privdata
				-- Ready is only sent by old versions of
				-- propellor. They expect to get privdata,
				-- and then no more protocol communication.
				Just Ready -> do
					sendprivdata toh privdata
					hClose toh
					
					-- Display remaining output.
					void $ tryIO $ forever $
						showremote =<< hGetLine fromh
					hClose fromh
				Nothing -> return ()
		loop
	
	user = "root@"++hn

	mkcmd = shellWrap . intercalate " ; "

	bootstrapcmd = mkcmd
		[ "if [ ! -d " ++ localdir ++ " ]"
		, "then " ++ intercalate " && "
			[ "apt-get update"
			, "apt-get --no-install-recommends --no-upgrade -y install git make"
			, "echo " ++ toMarked statusMarker (show NeedGitClone)
			]
		, "else " ++ intercalate " && "
			[ "cd " ++ localdir
			, "if ! test -x ./propellor; then make deps build; fi"
			, "./propellor --boot " ++ hn
			]
		, "fi"
		]

	runcmd = mkcmd
		[ "cd " ++ localdir ++ " && ./propellor --run " ++ hn ]

	showremote s = putStrLn s

	sendprivdata toh privdata = void $
		actionMessage ("Sending privdata (" ++ show (length privdata) ++ " bytes) to " ++ hn) $ do
			sendMarked toh privDataMarker privdata
			return True

-- Initial git clone, used for bootstrapping.
sendGitClone :: HostName -> IO ()
sendGitClone hn = void $ actionMessage ("Pushing git repository to " ++ hn) $ do
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
		]

-- Called "boot" for historical reasons, but what this really does is
-- update the privdata, repo url, and git repo over the ssh connection from the
-- client that ran propellor --spin.
boot :: IO ()
boot = do
	req NeedRepoUrl repoUrlMarker setRepoUrl
	makePrivDataDir
	req NeedPrivData privDataMarker $
		writeFileProtected privDataLocal
	req NeedGitPush gitPushMarker $ \_ -> do
		hin <- dup stdInput
		hout <- dup stdOutput
		hClose stdin
		hClose stdout
		unlessM (boolSystem "git" [Param "pull", Param "--upload-pack", Param $ "./propellor --gitpush " ++ show hin ++ " " ++ show hout, Param "."]) $
			errorMessage "git pull from client failed"

-- Shim for git push over the propellor ssh channel.
-- Reads from stdin and sends it to hout;
-- reads from hin and sends it to stdout.
gitPush :: Fd -> Fd -> IO ()
gitPush hin hout = void $ fromstdin `concurrently` tostdout
  where
	fromstdin = do
		h <- fdToHandle hout
		connect stdin h
	tostdout = do
		h <- fdToHandle hin
		connect h stdout
	connect fromh toh = do
		hSetBinaryMode fromh True
		hSetBinaryMode toh True
		b <- B.hGetSome fromh 40960
		if B.null b
			then do
				hClose fromh
				hClose toh
			else do
				B.hPut toh b
				hFlush toh
				connect fromh toh

hasOrigin :: IO Bool
hasOrigin = do
	rs <- lines <$> readProcess "git" ["remote"]
	return $ "origin" `elem` rs

setRepoUrl :: String -> IO ()
setRepoUrl "" = return ()
setRepoUrl url = do
	subcmd <- ifM hasOrigin (pure "set-url", pure "add")
	void $ boolSystem "git" [Param "remote", Param subcmd, Param "origin", Param url]
	-- same as --set-upstream-to, except origin branch
	-- may not have been pulled yet
	branch <- getCurrentBranch
	let branchval s = "branch." ++ branch ++ "." ++ s
	void $ boolSystem "git" [Param "config", Param (branchval "remote"), Param "origin"]
	void $ boolSystem "git" [Param "config", Param (branchval "merge"), Param $ "refs/heads/"++branch]

getRepoUrl :: IO (Maybe String)
getRepoUrl = getM get urls
  where
	urls = ["remote.deploy.url", "remote.origin.url"]
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
	go (Just "1") = do
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

module Propellor.CmdLine where

import System.Environment (getArgs)
import Data.List
import System.Exit
import System.PosixCompat
import Control.Exception (bracket)
import System.Posix.IO
import Control.Concurrent.Async
import qualified Data.ByteString as B
import System.Process (std_in, std_out)

import Propellor
import Propellor.Protocol
import Propellor.PrivData.Paths
import Propellor.Gpg
import Propellor.Git
import Propellor.Ssh
import qualified Propellor.Property.Docker as Docker
import qualified Propellor.Property.Docker.Shim as DockerShim
import Utility.FileMode
import Utility.SafeCommand

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
	go ("--update":h:[]) = return $ Update h
	go ("--boot":h:[]) = return $ Update h -- for back-compat
	go ("--run":h:[]) = return $ Run h
	go ("--add-key":k:[]) = return $ AddKey k
	go ("--set":f:c:[]) = withprivfield f c Set
	go ("--dump":f:c:[]) = withprivfield f c Dump
	go ("--edit":f:c:[]) = withprivfield f c Edit
	go ("--list-fields":[]) = return ListFields
	go ("--continue":s:[]) = case readish s of
		Just cmdline -> return $ Continue cmdline
		Nothing -> errorMessage "--continue serialization failure"
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
	go _ (Chain hn isconsole) = withhost hn $ \h -> do
		when isconsole forceConsole
		r <- runPropellor h $ ensureProperties $ hostProperties h
		putStrLn $ "\n" ++ show r
	go _ (Docker hn) = Docker.chain hn
	go _ (GitPush fin fout) = gitPush fin fout
	go True cmdline@(Spin _) = buildFirst cmdline $ go False cmdline
	go True cmdline = updateFirst cmdline $ go False cmdline
	go False (Spin hn) = withhost hn $ spin hn
	go False cmdline@(SimpleRun hn) = buildFirst cmdline $
		go False (Run hn)
	go False (Run hn) = ifM ((==) 0 <$> getRealUserID)
		( onlyProcess $ withhost hn mainProperties
		, go True (Spin hn)
		)
	go False (Update _) = do
		forceConsole
		onlyProcess update

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

updateFirst :: CmdLine -> IO () -> IO ()
updateFirst cmdline next = ifM hasOrigin (updateFirst' cmdline next, next)

updateFirst' :: CmdLine -> IO () -> IO ()
updateFirst' cmdline next = do
	branchref <- getCurrentBranch
	let originbranch = "origin" </> branchref

	void $ actionMessage "Git fetch" $ boolSystem "git" [Param "fetch"]
	
	oldsha <- getCurrentGitSha1 branchref
	
	whenM (doesFileExist keyring) $
		ifM (verifyOriginBranch originbranch)
			( do
				putStrLn $ "git branch " ++ originbranch ++ " gpg signature verified; merging"
				hFlush stdout
				void $ boolSystem "git" [Param "merge", Param originbranch]
			, warningMessage $ "git branch " ++ originbranch ++ " is not signed with a trusted gpg key; refusing to deploy it! (Running with previous configuration instead.)"
			)
	
	newsha <- getCurrentGitSha1 branchref

	if oldsha == newsha
		then next
		else ifM (actionMessage "Propellor build" $ boolSystem "make" [Param "build"])
			( void $ boolSystem "./propellor" [Param "--continue", Param (show cmdline)]
			, errorMessage "Propellor build failed!" 
			)

-- spin handles deploying propellor to a remote host, if it's not already
-- installed there, or updating it if it is. Once the remote propellor is
-- updated, it's run.
spin :: HostName -> Host -> IO ()
spin hn hst = do
	void $ actionMessage "Git commit (signed)" $
		gitCommit [Param "--allow-empty", Param "-a", Param "-m", Param "propellor spin"]
	-- Push to central origin repo first, if possible.
	-- The remote propellor will pull from there, which avoids
	-- us needing to send stuff directly to the remote host.
	whenM hasOrigin $
		void $ actionMessage "Push to central git repository" $
			boolSystem "git" [Param "push"]
	
	cacheparams <- toCommand <$> sshCachingParams hn
	comm cacheparams =<< hostprivdata
	unlessM (boolSystem "ssh" (map Param (cacheparams ++ ["-t", user, runcmd]))) $
		error $ "remote propellor failed (running: " ++ runcmd ++")"
  where
	hostprivdata = show . filterPrivData hst <$> decryptPrivData

	comm cacheparams privdata = 
		withBothHandles createProcessSuccess
			(proc "ssh" $ cacheparams ++ [ user, bootstrapcmd])
			(comm' cacheparams privdata)
	comm' cacheparams privdata (toh, fromh) = loop
	  where
		loop = dispatch =<< (maybe Nothing readish <$> getMarked fromh statusMarker)
		dispatch (Just NeedRepoUrl) = do
			sendMarked toh repoUrlMarker
				=<< (fromMaybe "" <$> getRepoUrl)
			loop
		dispatch (Just NeedPrivData) = do
			sendprivdata toh privdata
			loop
		dispatch (Just NeedGitPush) = do
			void $ actionMessage ("Sending git update to " ++ hn) $ do
				sendMarked toh gitPushMarker ""
				let p = (proc "git" ["upload-pack", "."])
					{ std_in = UseHandle fromh
					, std_out = UseHandle toh
					}
				(Nothing, Nothing, Nothing, h) <- createProcess p
				r <- waitForProcess h
				-- no more protocol possible after git push
				hClose fromh
				hClose toh
				return (r == ExitSuccess)
		dispatch (Just NeedGitClone) = do
			hClose toh
			hClose fromh
			sendGitClone hn
			comm cacheparams privdata
		dispatch Nothing = return ()
	
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
		[ "cd " ++ localdir ++ " && ./propellor --continue " ++ shellEscape (show (SimpleRun hn)) ]

	sendprivdata toh privdata = void $
		actionMessage ("Sending privdata (" ++ show (length privdata) ++ " bytes) to " ++ hn) $ do
			sendMarked toh privDataMarker privdata
			return True

-- Initial git clone, used for bootstrapping.
sendGitClone :: HostName -> IO ()
sendGitClone hn = void $ actionMessage ("Cloning git repository to " ++ hn) $ do
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

-- Update the privdata, repo url, and git repo over the ssh
-- connection from the client that ran propellor --spin.
update :: IO ()
update = do
	req NeedRepoUrl repoUrlMarker setRepoUrl
	makePrivDataDir
	req NeedPrivData privDataMarker $
		writeFileProtected privDataLocal
	req NeedGitPush gitPushMarker $ \_ -> do
		hin <- dup stdInput
		hout <- dup stdOutput
		hClose stdin
		hClose stdout
		unlessM (boolSystem "git" [Param "pull", Param "--progress", Param "--upload-pack", Param $ "./propellor --continue " ++ show (GitPush hin hout), Param "."]) $
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

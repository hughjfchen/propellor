-- When propellor --spin is running, the local host acts as a server,
-- which connects to the remote host's propellor and responds to its
-- requests.

module Propellor.Server (
	update,
	updateServer,
	gitPushHelper
) where

import Data.List
import System.Exit
import System.PosixCompat
import System.Posix.IO
import System.Posix.Directory
import Control.Concurrent.Async
import Control.Exception (bracket)
import qualified Data.ByteString as B

import Propellor
import Propellor.Protocol
import Propellor.PrivData.Paths
import Propellor.Git
import Propellor.Ssh
import qualified Propellor.Shim as Shim
import Utility.FileMode
import Utility.SafeCommand

-- Update the privdata, repo url, and git repo over the ssh
-- connection, talking to the user's local propellor instance which is
-- running the updateServer
update :: Maybe HostName -> IO ()
update forhost = do
	whenM hasGitRepo $
		req NeedRepoUrl repoUrlMarker setRepoUrl

	makePrivDataDir
	createDirectoryIfMissing True (takeDirectory privfile)
	req NeedPrivData privDataMarker $
		writeFileProtected privfile

	whenM hasGitRepo $
		req NeedGitPush gitPushMarker $ \_ -> do
			hin <- dup stdInput
			hout <- dup stdOutput
			hClose stdin
			hClose stdout
			unlessM (boolSystem "git" (pullparams hin hout)) $
				errorMessage "git pull from client failed"
  where
	pullparams hin hout =
		[ Param "pull"
		, Param "--progress"
		, Param "--upload-pack"
		, Param $ "./propellor --gitpush " ++ show hin ++ " " ++ show hout
		, Param "."
		]
	
	-- When --spin --relay is run, get a privdata file
	-- to be relayed to the target host.
	privfile = maybe privDataLocal privDataRelay forhost

-- The connect action should ssh to the remote host and run the provided
-- calback action.
updateServer :: HostName -> Maybe HostName -> Host -> (((Handle, Handle) -> IO ()) -> IO ()) -> IO ()
updateServer target relay hst connect = connect go
  where
	hn = fromMaybe target relay
	relaying = relay == Just target

	go (toh, fromh) = do
		let loop = go (toh, fromh)
		let restart = updateServer hn relay hst connect
		let done = return ()
		v <- (maybe Nothing readish <$> getMarked fromh statusMarker)
		case v of
			(Just NeedRepoUrl) -> do
				sendRepoUrl toh
				loop
			(Just NeedPrivData) -> do
				sendPrivData hn hst toh relaying
				loop
			(Just NeedGitClone) -> do
				hClose toh
				hClose fromh
				sendGitClone hn
				restart
			(Just NeedPrecompiled) -> do
				hClose toh
				hClose fromh
				sendPrecompiled hn
				restart
			(Just NeedGitPush) -> do
				sendGitUpdate hn fromh toh
				hClose fromh
				hClose toh
				done
			Nothing -> done

sendRepoUrl :: Handle -> IO ()
sendRepoUrl toh = sendMarked toh repoUrlMarker =<< (fromMaybe "" <$> getRepoUrl)

sendPrivData :: HostName -> Host -> Handle -> Bool -> IO ()
sendPrivData hn hst toh relaying = do
	privdata <- getdata
	void $ actionMessage ("Sending privdata (" ++ show (length privdata) ++ " bytes) to " ++ hn) $ do
		sendMarked toh privDataMarker privdata
		return True
  where
	getdata
		| relaying = do
			let f = privDataRelay hn
			d <- readFileStrictAnyEncoding f
			nukeFile f
			return d
		| otherwise = show . filterPrivData hst <$> decryptPrivData

sendGitUpdate :: HostName -> Handle -> Handle -> IO ()
sendGitUpdate hn fromh toh =
	void $ actionMessage ("Sending git update to " ++ hn) $ do
		sendMarked toh gitPushMarker ""
		(Nothing, Nothing, Nothing, h) <- createProcess p
		(==) ExitSuccess <$> waitForProcess h
  where
	p = (proc "git" ["upload-pack", "."])
		{ std_in = UseHandle fromh
		, std_out = UseHandle toh
		}

-- Initial git clone, used for bootstrapping.
sendGitClone :: HostName -> IO ()
sendGitClone hn = void $ actionMessage ("Clone git repository to " ++ hn) $ do
	branch <- getCurrentBranch
	cacheparams <- sshCachingParams hn False
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

-- Send a tarball containing the precompiled propellor, and libraries.
-- This should be reasonably portable, as long as the remote host has the
-- same architecture as the build host.
sendPrecompiled :: HostName -> IO ()
sendPrecompiled hn = void $ actionMessage ("Uploading locally compiled propellor as a last resort") $ do
	bracket getWorkingDirectory changeWorkingDirectory $ \_ ->
		withTmpDir "propellor" go
  where
	go tmpdir = do
		cacheparams <- sshCachingParams hn False
		let shimdir = takeFileName localdir
		createDirectoryIfMissing True (tmpdir </> shimdir)
		changeWorkingDirectory (tmpdir </> shimdir)
		me <- readSymbolicLink "/proc/self/exe"
		shim <- Shim.setup me "."
		when (shim /= "propellor") $
			renameFile shim "propellor"
		changeWorkingDirectory tmpdir
		withTmpFile "propellor.tar." $ \tarball _ -> allM id
			[ boolSystem "strip" [File me]
			, boolSystem "tar" [Param "czf", File tarball, File shimdir]
			, boolSystem "scp" $ cacheparams ++ [File tarball, Param ("root@"++hn++":"++remotetarball)]
			, boolSystem "ssh" $ cacheparams ++ [Param ("root@"++hn), Param unpackcmd]
			]

	remotetarball = "/usr/local/propellor.tar"

	unpackcmd = shellWrap $ intercalate " && "
		[ "cd " ++ takeDirectory remotetarball
		, "tar xzf " ++ remotetarball
		, "rm -f " ++ remotetarball
		]

-- Shim for git push over the propellor ssh channel.
-- Reads from stdin and sends it to hout;
-- reads from hin and sends it to stdout.
gitPushHelper :: Fd -> Fd -> IO ()
gitPushHelper hin hout = void $ fromstdin `concurrently` tostdout
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

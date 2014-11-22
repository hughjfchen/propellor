module Propellor.CmdLine (
	defaultMain,
	processCmdLine,
) where

import System.Environment (getArgs)
import Data.List
import System.Exit
import System.PosixCompat

import Propellor
import Propellor.Protocol
import Propellor.Gpg
import Propellor.Git
import Propellor.Ssh
import Propellor.Server
import qualified Propellor.Property.Docker as Docker
import qualified Propellor.Property.Chroot as Chroot
import qualified Propellor.Shim as Shim
import Utility.SafeCommand

usage :: Handle -> IO ()
usage h = hPutStrLn h $ unlines 
	[ "Usage:"
	, "  propellor"
	, "  propellor hostname"
	, "  propellor --spin targethost [--via relayhost]"
	, "  propellor --add-key keyid"
	, "  propellor --set field context"
	, "  propellor --dump field context"
	, "  propellor --edit field context"
	, "  propellor --list-fields"
	]

usageError :: [String] -> IO a
usageError ps = do
	usage stderr
	error ("(Unexpected: " ++ show ps)

processCmdLine :: IO CmdLine
processCmdLine = go =<< getArgs
  where
	go ("--run":h:[]) = return $ Run h
	go ("--spin":h:[]) = return $ Spin h Nothing
	go ("--spin":h:"--via":r:[]) = return $ Spin h (Just r)
	go ("--add-key":k:[]) = return $ AddKey k
	go ("--set":f:c:[]) = withprivfield f c Set
	go ("--dump":f:c:[]) = withprivfield f c Dump
	go ("--edit":f:c:[]) = withprivfield f c Edit
	go ("--list-fields":[]) = return ListFields
	go ("--help":_) = do	
		usage stdout
		exitFailure
	go ("--update":_:[]) = return $ Update Nothing
	go ("--boot":_:[]) = return $ Update Nothing -- for back-compat
	go ("--continue":s:[]) = case readish s of
		Just cmdline -> return $ Continue cmdline
		Nothing -> errorMessage $ "--continue serialization failure (" ++ s ++ ")"
	go ("--gitpush":fin:fout:_) = return $ GitPush (Prelude.read fin) (Prelude.read fout)
	go (h:[])
		| "--" `isPrefixOf` h = usageError [h]
		| otherwise = return $ Run h
	go [] = do
		s <- takeWhile (/= '\n') <$> readProcess "hostname" ["-f"]
		if null s
			then errorMessage "Cannot determine hostname! Pass it on the command line."
			else return $ Run s
	go v = usageError v

	withprivfield s c f = case readish s of
		Just pf -> return $ f pf (Context c)
		Nothing -> errorMessage $ "Unknown privdata field " ++ s

-- | Runs propellor on hosts, as controlled by command-line options.
defaultMain :: [Host] -> IO ()
defaultMain hostlist = do
	Shim.cleanEnv
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
	go _ c@(ChrootChain _ _ _ _) = Chroot.chain hostlist c
	go _ (DockerChain hn cid) = Docker.chain hostlist hn cid
	go _ (DockerInit hn) = Docker.init hn
	go _ (GitPush fin fout) = gitPushHelper fin fout
	go _ (Update Nothing) = forceConsole >> fetchFirst (onlyprocess (update Nothing))
	go _ (Update (Just h)) = forceConsole >> fetchFirst (update (Just h))
	go True cmdline@(Spin _ _) = buildFirst cmdline $ go False cmdline
	go True cmdline = updateFirst cmdline $ go False cmdline
	go False (Spin hn r) = withhost hn $ spin hn r
	go False cmdline@(SimpleRun hn) = buildFirst cmdline $
		go False (Run hn)
	go False (Run hn) = ifM ((==) 0 <$> getRealUserID)
		( onlyprocess $ withhost hn mainProperties
		, go True (Spin hn Nothing)
		)

	withhost :: HostName -> (Host -> IO ()) -> IO ()
	withhost hn a = maybe (unknownhost hn hostlist) a (findHost hostlist hn)
	
	onlyprocess = onlyProcess (localdir </> ".lock")

unknownhost :: HostName -> [Host] -> IO a
unknownhost h hosts = errorMessage $ unlines
	[ "Propellor does not know about host: " ++ h
	, "(Perhaps you should specify the real hostname on the command line?)"
	, "(Or, edit propellor's config.hs to configure this host)"
	, "Known hosts: " ++ unwords (map hostName hosts)
	]

buildFirst :: CmdLine -> IO () -> IO ()
buildFirst cmdline next = ifM (doesFileExist "Makefile")
	( do
		oldtime <- getmtime
		ifM (actionMessage "Propellor build" $ boolSystem "make" [Param "build"])
			( do
				newtime <- getmtime
				if newtime == oldtime
					then next
					else void $ boolSystem "./propellor" [Param "--continue", Param (show cmdline)]
			, errorMessage "Propellor build failed!" 
			)
	, next
	)
  where
	getmtime = catchMaybeIO $ getModificationTime "propellor"

fetchFirst :: IO () -> IO ()
fetchFirst next = do
	whenM hasOrigin $
		void fetchOrigin
	next

updateFirst :: CmdLine -> IO () -> IO ()
updateFirst cmdline next = ifM hasOrigin (updateFirst' cmdline next, next)

updateFirst' :: CmdLine -> IO () -> IO ()
updateFirst' cmdline next = ifM fetchOrigin
	( ifM (actionMessage "Propellor build" $ boolSystem "make" [Param "build"])
		( void $ boolSystem "./propellor" [Param "--continue", Param (show cmdline)]
			, errorMessage "Propellor build failed!" 
		)
	, next
	)

spin :: HostName -> Maybe HostName -> Host -> IO ()
spin target relay hst = do
	void $ actionMessage "Git commit" $
		gitCommit [Param "--allow-empty", Param "-a", Param "-m", Param "propellor spin"]
	-- Push to central origin repo first, if possible.
	-- The remote propellor will pull from there, which avoids
	-- us needing to send stuff directly to the remote host.
	whenM hasOrigin $
		void $ actionMessage "Push to central git repository" $
			boolSystem "git" [Param "push"]
	
	cacheparams <- toCommand <$> sshCachingParams hn
	when (isJust relay) $
		void $ boolSystem "ssh-add" []

	-- Install, or update the remote propellor.
	updateServer target relay hst $ withBothHandles createProcessSuccess
		(proc "ssh" $ cacheparams ++ [user, updatecmd])

	-- And now we can run it.
	unlessM (boolSystem "ssh" (map Param $ cacheparams ++ ["-t", user, runcmd])) $
		error $ "remote propellor failed"
  where
	hn = fromMaybe target relay
	user = "root@"++hn

	mkcmd = shellWrap . intercalate " ; "

	updatecmd = mkcmd
		[ "if [ ! -d " ++ localdir ++ " ]"
		, "then (" ++ intercalate " && "
			[ "if ! git --version || ! make --version; the apt-get update && apt-get --no-install-recommends --no-upgrade -y install git make; fi"
			, "echo " ++ toMarked statusMarker (show NeedGitClone)
			] ++ ") || echo " ++ toMarked statusMarker (show NeedPrecompiled)
		, "else " ++ intercalate " && "
			[ "cd " ++ localdir
			, "if ! test -x ./propellor; then make deps build; fi"
			, if isNothing relay
				-- Still using --boot for back-compat...
				then "./propellor --boot " ++ target
				else "./propellor --continue " ++
					shellEscape (show (Update (Just target)))
			]
		, "fi"
		]

	runcmd = mkcmd [ "cd " ++ localdir ++ " && ./propellor " ++ cmd ]
	cmd = if isNothing relay
		then "--continue " ++ shellEscape (show (SimpleRun target))
		else "--spin " ++ shellEscape target


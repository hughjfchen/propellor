module Propellor.CmdLine (
	defaultMain,
	processCmdLine,
) where

import System.Environment (getArgs)
import Data.List
import System.Exit
import System.PosixCompat
import Network.Socket

import Propellor
import Propellor.Gpg
import Propellor.Git
import Propellor.Bootstrap
import Propellor.Spin
import Propellor.Types.CmdLine
import qualified Propellor.Property.Docker as Docker
import qualified Propellor.Property.Chroot as Chroot
import qualified Propellor.Shim as Shim

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
	, "  propellor --merge"
	, "  propellor --build"
	, "  propellor --check"
	]

usageError :: [String] -> IO a
usageError ps = do
	usage stderr
	error ("(Unexpected: " ++ show ps)

processCmdLine :: IO CmdLine
processCmdLine = go =<< getArgs
  where
  	go ("--check":_) = return Check
	go ("--spin":ps) = case reverse ps of
		(r:"--via":hs) -> Spin 
			<$> mapM hostname (reverse hs) 
			<*> pure (Just r)
		_ -> Spin <$> mapM hostname ps <*> pure Nothing
	go ("--add-key":k:[]) = return $ AddKey k
	go ("--set":f:c:[]) = withprivfield f c Set
	go ("--dump":f:c:[]) = withprivfield f c Dump
	go ("--edit":f:c:[]) = withprivfield f c Edit
	go ("--list-fields":[]) = return ListFields
	go ("--merge":[]) = return Merge
	go ("--help":_) = do	
		usage stdout
		exitFailure
	go ("--boot":_:[]) = return $ Update Nothing -- for back-compat
	go ("--serialized":s:[]) = serialized Serialized s
	go ("--continue":s:[]) = serialized Continue s
	go ("--gitpush":fin:fout:_) = return $ GitPush (Prelude.read fin) (Prelude.read fout)
	go ("--run":h:[]) = go [h]
	go (h:[])
		| "--" `isPrefixOf` h = usageError [h]
		| otherwise = Run <$> hostname h
	go [] = do
		s <- takeWhile (/= '\n') <$> readProcess "hostname" ["-f"]
		if null s
			then errorMessage "Cannot determine hostname! Pass it on the command line."
			else return $ Run s
	go v = usageError v

	withprivfield s c f = case readish s of
		Just pf -> return $ f pf (Context c)
		Nothing -> errorMessage $ "Unknown privdata field " ++ s

	serialized mk s = case readish s of
		Just cmdline -> return $ mk cmdline
		Nothing -> errorMessage $ "serialization failure (" ++ s ++ ")"

-- | Runs propellor on hosts, as controlled by command-line options.
defaultMain :: [Host] -> IO ()
defaultMain hostlist = do
	Shim.cleanEnv
	checkDebugMode
	cmdline <- processCmdLine
	debug ["command line: ", show cmdline]
	go True cmdline
  where
	go _ (Serialized cmdline) = go True cmdline
	go _ (Continue cmdline) = go False cmdline
	go _ Check = return ()
	go _ (Set field context) = setPrivData field context
	go _ (Dump field context) = dumpPrivData field context
	go _ (Edit field context) = editPrivData field context
	go _ ListFields = listPrivDataFields hostlist
	go _ (AddKey keyid) = addKey keyid
	go _ c@(ChrootChain _ _ _ _) = Chroot.chain hostlist c
	go _ (DockerChain hn cid) = Docker.chain hostlist hn cid
	go _ (DockerInit hn) = Docker.init hn
	go _ (GitPush fin fout) = gitPushHelper fin fout
	go _ (Relay h) = forceConsole >> updateFirst (Update (Just h)) (update (Just h))
	go _ (Update Nothing) = forceConsole >> fetchFirst (onlyprocess (update Nothing))
	go _ (Update (Just h)) = update (Just h)
	go _ Merge = mergeSpin
	go True cmdline@(Spin _ _) = buildFirst cmdline $ go False cmdline
	go True cmdline = updateFirst cmdline $ go False cmdline
	go False (Spin hs r) = do
		commitSpin
		forM_ hs $ \hn -> withhost hn $ spin hn r
	go False cmdline@(SimpleRun hn) = buildFirst cmdline $
		go False (Run hn)
	go False (Run hn) = ifM ((==) 0 <$> getRealUserID)
		( onlyprocess $ withhost hn mainProperties
		, go True (Spin [hn] Nothing)
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
buildFirst cmdline next = do
	oldtime <- getmtime
	buildPropellor
	newtime <- getmtime
	if newtime == oldtime
		then next
		else void $ boolSystem "./propellor"
			[ Param "--continue"
			, Param (show cmdline)
			]
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
	( do
		buildPropellor
		void $ boolSystem "./propellor"
			[ Param "--continue"
			, Param (show cmdline)
			]
	, next
	)

-- Gets the fully qualified domain name, given a string that might be
-- a short name to look up in the DNS.
hostname :: String -> IO HostName
hostname s = go =<< catchDefaultIO [] dnslookup
  where
	dnslookup = getAddrInfo (Just canonname) (Just s) Nothing
	canonname = defaultHints { addrFlags = [AI_CANONNAME] }
	go (AddrInfo { addrCanonName = Just v } : _) = pure v
	go _
		| "." `isInfixOf` s = pure s -- assume it's a fqdn
		| otherwise = 
			error $ "cannot find host " ++ s ++ " in the DNS"

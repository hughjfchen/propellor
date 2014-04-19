{-# LANGUAGE BangPatterns #-}

-- | Docker support for propellor
--
-- The existance of a docker container is just another Property of a system,
-- which propellor can set up. See config.hs for an example.

module Propellor.Property.Docker where

import Propellor
import Propellor.SimpleSh
import Propellor.Types.Attr
import qualified Propellor.Property.File as File
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.Docker.Shim as Shim
import Utility.SafeCommand
import Utility.Path

import Control.Concurrent.Async
import System.Posix.Directory
import System.Posix.Process
import Data.List
import Data.List.Utils

-- | Configures docker with an authentication file, so that images can be
-- pushed to index.docker.io.
configured :: Property
configured = property "docker configured" go `requires` installed
  where
	go = withPrivData DockerAuthentication $ \cfg -> ensureProperty $ 
		"/root/.dockercfg" `File.hasContent` (lines cfg)

installed :: Property
installed = Apt.installed ["docker.io"]

-- | A short descriptive name for a container.
-- Should not contain whitespace or other unusual characters,
-- only [a-zA-Z0-9_-] are allowed
type ContainerName = String

-- | Starts accumulating the properties of a Docker container.
--
-- > container "web-server" "debian"
-- >    & publish "80:80"
-- >    & Apt.installed {"apache2"]
-- >    & ...
container :: ContainerName -> Image -> Host
container cn image = Host [] (\_ -> attr)
  where
	attr = (newAttr (cn2hn cn)) { _dockerImage = Just image }

cn2hn :: ContainerName -> HostName
cn2hn cn = cn ++ ".docker"

-- | Ensures that a docker container is set up and running. The container
-- has its own Properties which are handled by running propellor
-- inside the container.
--
-- Reverting this property ensures that the container is stopped and
-- removed.
docked
	:: [Host]
	-> ContainerName
	-> RevertableProperty
docked hosts cn = RevertableProperty (go "docked" setup) (go "undocked" teardown)
  where
	go desc a = property (desc ++ " " ++ cn) $ do
		hn <- getHostName
  		let cid = ContainerId hn cn
		ensureProperties [findContainer hosts cid cn $ a cid]

	setup cid (Container image runparams) =
		provisionContainer cid
			`requires`
		runningContainer cid image runparams
			`requires`
		installed

	teardown cid (Container image _runparams) =
		combineProperties ("undocked " ++ fromContainerId cid)
			[ stoppedContainer cid
			, property ("cleaned up " ++ fromContainerId cid) $
				liftIO $ report <$> mapM id
					[ removeContainer cid
					, removeImage image
					]
			]

findContainer
	:: [Host]
	-> ContainerId
	-> ContainerName
	-> (Container -> Property)
	-> Property
findContainer hosts cid cn mk = case findHost hosts (cn2hn cn) of
	Nothing -> cantfind
	Just h -> maybe cantfind mk (mkContainer cid h)
  where
	cantfind = containerDesc cid $ property "" $ do
		liftIO $ warningMessage $
			"missing definition for docker container \"" ++ cn2hn cn
		return FailedChange

mkContainer :: ContainerId -> Host -> Maybe Container
mkContainer cid@(ContainerId hn _cn) h = Container
	<$> _dockerImage attr
	<*> pure (map (\a -> a hn) (_dockerRunParams attr))
  where
	attr = hostAttr h'
  	h' = h
		-- expose propellor directory inside the container
		& volume (localdir++":"++localdir)
		-- name the container in a predictable way so we
		-- and the user can easily find it later
		& name (fromContainerId cid)

-- | Causes *any* docker images that are not in use by running containers to
-- be deleted. And deletes any containers that propellor has set up
-- before that are not currently running. Does not delete any containers
-- that were not set up using propellor.
--
-- Generally, should come after the properties for the desired containers.
garbageCollected :: Property
garbageCollected = propertyList "docker garbage collected"
	[ gccontainers
	, gcimages
	]
  where
	gccontainers = property "docker containers garbage collected" $
		liftIO $ report <$> (mapM removeContainer =<< listContainers AllContainers)
	gcimages = property "docker images garbage collected" $ do
		liftIO $ report <$> (mapM removeImage =<< listImages)

data Container = Container Image [RunParam]

-- | Parameters to pass to `docker run` when creating a container.
type RunParam = String

-- | A docker image, that can be used to run a container.
type Image = String

-- | Set custom dns server for container.
dns :: String -> Property
dns = runProp "dns"

-- | Set container host name.
hostname :: String -> Property
hostname = runProp "hostname"

-- | Set name for container. (Normally done automatically.)
name :: String -> Property
name = runProp "name"

-- | Publish a container's port to the host
-- (format: ip:hostPort:containerPort | ip::containerPort | hostPort:containerPort)
publish :: String -> Property
publish = runProp "publish"

-- | Username or UID for container.
user :: String -> Property
user = runProp "user"

-- | Mount a volume
-- Create a bind mount with: [host-dir]:[container-dir]:[rw|ro]
-- With just a directory, creates a volume in the container.
volume :: String -> Property
volume = runProp "volume"

-- | Mount a volume from the specified container into the current
-- container.
volumes_from :: ContainerName -> Property
volumes_from cn = genProp "volumes-from" $ \hn ->
	fromContainerId (ContainerId hn cn)

-- | Work dir inside the container. 
workdir :: String -> Property
workdir = runProp "workdir"

-- | Memory limit for container.
--Format: <number><optional unit>, where unit = b, k, m or g
memory :: String -> Property
memory = runProp "memory"

-- | Link with another container on the same host.
link :: ContainerName -> ContainerAlias -> Property
link linkwith calias = genProp "link" $ \hn ->
	fromContainerId (ContainerId hn linkwith) ++ ":" ++ calias

-- | A short alias for a linked container.
-- Each container has its own alias namespace.
type ContainerAlias = String

-- | A container is identified by its name, and the host
-- on which it's deployed.
data ContainerId = ContainerId HostName ContainerName
	deriving (Eq, Read, Show)

-- | Two containers with the same ContainerIdent were started from
-- the same base image (possibly a different version though), and
-- with the same RunParams.
data ContainerIdent = ContainerIdent Image HostName ContainerName [RunParam]
	deriving (Read, Show, Eq)

ident2id :: ContainerIdent -> ContainerId
ident2id (ContainerIdent _ hn cn _) = ContainerId hn cn

toContainerId :: String -> Maybe ContainerId
toContainerId s
	| myContainerSuffix `isSuffixOf` s = case separate (== '.') (desuffix s) of
		(cn, hn)
			| null hn || null cn -> Nothing
			| otherwise -> Just $ ContainerId hn cn
	| otherwise = Nothing
  where
	desuffix = reverse . drop len . reverse
	len = length myContainerSuffix

fromContainerId :: ContainerId -> String
fromContainerId (ContainerId hn cn) = cn++"."++hn++myContainerSuffix

containerHostName :: ContainerId -> HostName
containerHostName (ContainerId _ cn) = cn2hn cn

myContainerSuffix :: String
myContainerSuffix = ".propellor"

containerDesc :: ContainerId -> Property -> Property
containerDesc cid p = p `describe` desc
  where
	desc = "[" ++ fromContainerId cid ++ "] " ++ propertyDesc p

runningContainer :: ContainerId -> Image -> [RunParam] -> Property
runningContainer cid@(ContainerId hn cn) image runps = containerDesc cid $ property "running" $ do
	l <- liftIO $ listContainers RunningContainers
	if cid `elem` l
		then do
			-- Check if the ident has changed; if so the
			-- parameters of the container differ and it must
			-- be restarted.
			runningident <- liftIO $ getrunningident
			if runningident == Just ident
				then noChange
				else do
					void $ liftIO $ stopContainer cid
					restartcontainer
		else ifM (liftIO $ elem cid <$> listContainers AllContainers)
			( restartcontainer
			, go image
			)
  where
	ident = ContainerIdent image hn cn runps

	restartcontainer = do
		oldimage <- liftIO $ fromMaybe image <$> commitContainer cid
		void $ liftIO $ removeContainer cid
		go oldimage

	getrunningident :: IO (Maybe ContainerIdent)
	getrunningident = simpleShClient (namedPipe cid) "cat" [propellorIdent] $ \rs -> do
		let !v = extractident rs
		return v

	extractident :: [Resp] -> Maybe ContainerIdent
	extractident = headMaybe . catMaybes . map readish . catMaybes . map getStdout

	go img = do
		liftIO $ do
			clearProvisionedFlag cid
			createDirectoryIfMissing True (takeDirectory $ identFile cid)
		shim <- liftIO $ Shim.setup (localdir </> "propellor") (localdir </> shimdir cid)
		liftIO $ writeFile (identFile cid) (show ident)
		ensureProperty $ boolProperty "run" $ runContainer img
			(runps ++ ["-i", "-d", "-t"])
			[shim, "--docker", fromContainerId cid]

-- | Called when propellor is running inside a docker container.
-- The string should be the container's ContainerId.
--
-- This process is effectively init inside the container.
-- It even needs to wait on zombie processes!
--
-- Fork a thread to run the SimpleSh server in the background.
-- In the foreground, run an interactive bash (or sh) shell,
-- so that the user can interact with it when attached to the container.
--
-- When the system reboots, docker restarts the container, and this is run
-- again. So, to make the necessary services get started on boot, this needs
-- to provision the container then. However, if the container is already
-- being provisioned by the calling propellor, it would be redundant and
-- problimatic to also provisoon it here.
--
-- The solution is a flag file. If the flag file exists, then the container
-- was already provisioned. So, it must be a reboot, and time to provision
-- again. If the flag file doesn't exist, don't provision here.
chain :: String -> IO ()
chain s = case toContainerId s of
	Nothing -> error $ "Invalid ContainerId: " ++ s
	Just cid -> do
		changeWorkingDirectory localdir
		writeFile propellorIdent . show =<< readIdentFile cid
		-- Run boot provisioning before starting simpleSh,
		-- to avoid ever provisioning twice at the same time.
		whenM (checkProvisionedFlag cid) $ do
			let shim = Shim.file (localdir </> "propellor") (localdir </> shimdir cid)
			unlessM (boolSystem shim [Param "--continue", Param $ show $ Chain $ containerHostName cid]) $
				warningMessage "Boot provision failed!"
		void $ async $ job reapzombies
		void $ async $ job $ simpleSh $ namedPipe cid
		job $ do
			void $ tryIO $ ifM (inPath "bash")
				( boolSystem "bash" [Param "-l"]
				, boolSystem "/bin/sh" []
				)
			putStrLn "Container is still running. Press ^P^Q to detach."
  where
	job = forever . void . tryIO
	reapzombies = void $ getAnyProcessStatus True False

-- | Once a container is running, propellor can be run inside
-- it to provision it.
--
-- Note that there is a race here, between the simplesh
-- server starting up in the container, and this property
-- being run. So, retry connections to the client for up to
-- 1 minute.
provisionContainer :: ContainerId -> Property
provisionContainer cid = containerDesc cid $ property "provision" $ liftIO $ do
	let shim = Shim.file (localdir </> "propellor") (localdir </> shimdir cid)
	r <- simpleShClientRetry 60 (namedPipe cid) shim params (go Nothing)
	when (r /= FailedChange) $
		setProvisionedFlag cid 
	return r
  where
	params = ["--continue", show $ Chain $ containerHostName cid]

	go lastline (v:rest) = case v of
		StdoutLine s -> do
			debug ["stdout: ", show s]
			maybe noop putStrLn lastline
			hFlush stdout
			go (Just s) rest
		StderrLine s -> do
			debug ["stderr: ", show s]
			maybe noop putStrLn lastline
			hFlush stdout
			hPutStrLn stderr s
			hFlush stderr
			go Nothing rest
		Done -> ret lastline
	go lastline [] = ret lastline

	ret lastline = return $ fromMaybe FailedChange $
		readish =<< lastline

stopContainer :: ContainerId -> IO Bool
stopContainer cid = boolSystem dockercmd [Param "stop", Param $ fromContainerId cid ]

stoppedContainer :: ContainerId -> Property
stoppedContainer cid = containerDesc cid $ property desc $ 
	ifM (liftIO $ elem cid <$> listContainers RunningContainers)
		( liftIO cleanup `after` ensureProperty 
			(boolProperty desc $ stopContainer cid)
		, return NoChange
		)
  where
	desc = "stopped"
	cleanup = do
		nukeFile $ namedPipe cid
		nukeFile $ identFile cid
		removeDirectoryRecursive $ shimdir cid
		clearProvisionedFlag cid

removeContainer :: ContainerId -> IO Bool
removeContainer cid = catchBoolIO $
	snd <$> processTranscript dockercmd ["rm", fromContainerId cid ] Nothing

removeImage :: Image -> IO Bool
removeImage image = catchBoolIO $
	snd <$> processTranscript dockercmd ["rmi", image ] Nothing

runContainer :: Image -> [RunParam] -> [String] -> IO Bool
runContainer image ps cmd = boolSystem dockercmd $ map Param $
	"run" : (ps ++ image : cmd)

commitContainer :: ContainerId -> IO (Maybe Image)
commitContainer cid = catchMaybeIO $
	takeWhile (/= '\n') 
		<$> readProcess dockercmd ["commit", fromContainerId cid]

data ContainerFilter = RunningContainers | AllContainers
	deriving (Eq)

-- | Only lists propellor managed containers.
listContainers :: ContainerFilter -> IO [ContainerId]
listContainers status = 
	catMaybes . map toContainerId . concat . map (split ",")
		. catMaybes . map (lastMaybe . words) . lines
		<$> readProcess dockercmd ps
  where
	ps
		| status == AllContainers = baseps ++ ["--all"]
		| otherwise = baseps
	baseps = ["ps", "--no-trunc"]

listImages :: IO [Image]
listImages = lines <$> readProcess dockercmd ["images", "--all", "--quiet"]

runProp :: String -> RunParam -> Property
runProp field val = pureAttrProperty (param) $ \attr ->
	attr { _dockerRunParams = _dockerRunParams attr ++ [\_ -> "--"++param] }
  where
	param = field++"="++val

genProp :: String -> (HostName -> RunParam) -> Property
genProp field mkval = pureAttrProperty field $ \attr ->
	attr { _dockerRunParams = _dockerRunParams attr ++ [\hn -> "--"++field++"=" ++ mkval hn] }

-- | The ContainerIdent of a container is written to
-- /.propellor-ident inside it. This can be checked to see if
-- the container has the same ident later.
propellorIdent :: FilePath
propellorIdent = "/.propellor-ident"

-- | Named pipe used for communication with the container.
namedPipe :: ContainerId -> FilePath
namedPipe cid = "docker" </> fromContainerId cid

provisionedFlag :: ContainerId -> FilePath
provisionedFlag cid = "docker" </> fromContainerId cid ++ ".provisioned"

clearProvisionedFlag :: ContainerId -> IO ()
clearProvisionedFlag = nukeFile . provisionedFlag

setProvisionedFlag :: ContainerId -> IO ()
setProvisionedFlag cid = do
	createDirectoryIfMissing True (takeDirectory (provisionedFlag cid))
	writeFile (provisionedFlag cid) "1"

checkProvisionedFlag :: ContainerId -> IO Bool
checkProvisionedFlag = doesFileExist . provisionedFlag

shimdir :: ContainerId -> FilePath
shimdir cid = "docker" </> fromContainerId cid ++ ".shim"

identFile :: ContainerId -> FilePath
identFile cid = "docker" </> fromContainerId cid ++ ".ident"

readIdentFile :: ContainerId -> IO ContainerIdent
readIdentFile cid = fromMaybe (error "bad ident in identFile")
	. readish <$> readFile (identFile cid)

dockercmd :: String
dockercmd = "docker.io"

report :: [Bool] -> Result
report rmed
	| or rmed = MadeChange
	| otherwise = NoChange


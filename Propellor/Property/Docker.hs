{-# LANGUAGE RankNTypes #-}

-- | Docker support for propellor
--
-- The existance of a docker container is just another Property of a system,
-- which propellor can set up. See config.hs for an example.
--
-- Note that propellor provisions a container by running itself, inside the
-- container. Currently, to avoid the overhead of building propellor
-- inside the container, the binary from outside is reused inside. 
-- So, the libraries that propellor is linked against need to be available
-- in the container with compatable versions. This can cause a problem
-- if eg, mixing Debian stable and unstable.

module Propellor.Property.Docker where

import Propellor
import Propellor.SimpleSh
import qualified Propellor.Property.File as File
import qualified Propellor.Property.Apt as Apt
import Utility.SafeCommand
import Utility.Path

import Control.Concurrent.Async
import System.Posix.Directory
import Data.List

-- | Configures docker with an authentication file, so that images can be
-- pushed to index.docker.io.
configured :: Property
configured = Property "docker configured" go `requires` installed
  where
	go = withPrivData DockerAuthentication $ \cfg -> ensureProperty $ 
		"/root/.dockercfg" `File.hasContent` (lines cfg)

installed :: Property
installed = Apt.installed ["docker.io"]

-- | Ensures that a docker container is set up and running. The container
-- has its own Properties which are handled by running propellor
-- inside the container.
--
-- Reverting this property ensures that the container is stopped and
-- removed.
docked
	:: (HostName -> ContainerName -> Maybe (Container))
	-> HostName
	-> ContainerName
	-> RevertableProperty
docked findc hn cn = findContainer findc hn cn $
	\(Container image containerprops) ->
		let setup = provisionContainer cid
				`requires`
			runningContainer cid image containerprops
				`requires`
			installed
		    teardown = combineProperties ("undocked " ++ fromContainerId cid)
		    	[ stoppedContainer cid
			, Property ("cleaned up " ++ fromContainerId cid) $
				report <$> mapM id
					[ removeContainer cid
					, removeImage image
					]
			]
		in RevertableProperty setup teardown
  where
  	cid = ContainerId hn cn

findContainer
	:: (HostName -> ContainerName -> Maybe (Container))
	-> HostName
	-> ContainerName
	-> (Container -> RevertableProperty)
	-> RevertableProperty
findContainer findc hn cn mk = case findc hn cn of
	Nothing -> RevertableProperty cantfind cantfind
	Just container -> mk container
  where
  	cid = ContainerId hn cn
	cantfind = containerDesc (ContainerId hn cn) $ Property "" $ do
		warningMessage $ "missing definition for docker container \"" ++ fromContainerId cid
		return FailedChange

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
	gccontainers = Property "docker containers garbage collected" $
		report <$> (mapM removeContainer =<< listContainers AllContainers)
	gcimages = Property "docker images garbage collected" $ do
		report <$> (mapM removeImage =<< listImages)

-- | Pass to defaultMain to add docker containers.
-- You need to provide the function mapping from
-- HostName and ContainerName to the Container to use.
containerProperties
	:: (HostName -> ContainerName -> Maybe (Container))
	-> (HostName -> Maybe [Property])
containerProperties findcontainer = \h -> case toContainerId h of
	Nothing -> Nothing
	Just cid@(ContainerId hn cn) ->
		case findcontainer hn cn of
			Nothing -> Nothing
			Just (Container _ cprops) -> 
				Just $ map (containerDesc cid) $
					fromContainerized cprops

-- | This type is used to configure a docker container.
-- It has an image, and a list of Properties, but these
-- properties are Containerized; they can specify
-- things about the container's configuration, in
-- addition to properties of the system inside the
-- container.
data Container = Container Image [Containerized Property]

data Containerized a = Containerized [RunParam] a

-- | Parameters to pass to `docker run` when creating a container.
type RunParam = String

-- | A docker image, that can be used to run a container.
type Image = String

-- | A short descriptive name for a container.
-- Should not contain whitespace or other unusual characters,
-- only [a-zA-Z0-9_.-] are allowed
type ContainerName = String

-- | Lift a Property to apply inside a container.
inside1 :: Property -> Containerized Property
inside1 = Containerized []

inside :: [Property] -> Containerized Property
inside = Containerized [] . combineProperties "provision"

-- | Set custom dns server for container.
dns :: String -> Containerized Property
dns = runProp "dns"

-- | Set container host name.
hostname :: String -> Containerized Property
hostname = runProp "hostname"

-- | Set name for container. (Normally done automatically.)
name :: String -> Containerized Property
name = runProp "name"

-- | Publish a container's port to the host
-- (format: ip:hostPort:containerPort | ip::containerPort | hostPort:containerPort)
publish :: String -> Containerized Property
publish = runProp "publish"

-- | Username or UID for container.
user :: String -> Containerized Property
user = runProp "user"

-- | Bind mount a volume
volume :: String -> Containerized Property
volume = runProp "volume"

-- | Work dir inside the container. 
workdir :: String -> Containerized Property
workdir = runProp "workdir"

-- | Memory limit for container.
--Format: <number><optional unit>, where unit = b, k, m or g
memory :: String -> Containerized Property
memory = runProp "memory"

-- | A container is identified by its name, and the host
-- on which it's deployed.
data ContainerId = ContainerId HostName ContainerName
	deriving (Eq, Read, Show)

-- | Two containers with the same ContainerIdent were started from
-- the same base image (possibly a different version though), and
-- with the same RunParams.
data ContainerIdent = ContainerIdent Image HostName ContainerName [RunParam]
	deriving (Read, Show, Eq)

getRunParams :: [Containerized a] -> [RunParam]
getRunParams l = concatMap get l
  where
	get (Containerized ps _) = ps

fromContainerized :: forall a. [Containerized a] -> [a]
fromContainerized l = map get l
  where
	get (Containerized _ a) = a

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

myContainerSuffix :: String
myContainerSuffix = ".propellor"

containerFrom :: Image -> [Containerized Property] -> Container
containerFrom = Container

containerDesc :: ContainerId -> Property -> Property
containerDesc cid p = p `describe` desc
  where
	desc = "[" ++ fromContainerId cid ++ "] " ++ propertyDesc p

runningContainer :: ContainerId -> Image -> [Containerized Property] -> Property
runningContainer cid@(ContainerId hn cn) image containerprops = containerDesc cid $ Property "running" $ do
	l <- listContainers RunningContainers
	if cid `elem` l
		then do
			runningident <- getrunningident
			if (ident2id <$> runningident) == Just (ident2id ident)
				then return NoChange
				else do
					void $ stopContainer cid
					oldimage <- fromMaybe image <$> commitContainer cid
					void $ removeContainer cid
					go oldimage
		else do
			whenM (elem cid <$> listContainers AllContainers) $ do
				void $ removeContainer cid
			go image
  where
	ident = ContainerIdent image hn cn runps

	getrunningident = catchDefaultIO Nothing $
		simpleShClient (namedPipe cid) "cat" [propellorIdent] $
			pure . headMaybe . catMaybes . map readish . catMaybes . map getStdout

	runps = getRunParams $ containerprops ++
		-- expose propellor directory inside the container
		[ volume (localdir++":"++localdir)
		-- name the container in a predictable way so we
		-- and the user can easily find it later
		, name (fromContainerId cid)
		]
	
	chaincmd = [localdir </> "propellor", "--docker", fromContainerId cid]

	go img = do
		clearProvisionedFlag cid
		createDirectoryIfMissing True (takeDirectory $ identFile cid)
		writeFile (identFile cid) (show ident)
		ensureProperty $ boolProperty "run" $ runContainer img
			(runps ++ ["-i", "-d", "-t"])
			chaincmd

-- | Called when propellor is running inside a docker container.
-- The string should be the container's ContainerId.
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
		whenM (checkProvisionedFlag cid) $
			unlessM (boolSystem "./propellor" [Param "--continue", Param $ show $ Chain $ fromContainerId cid]) $
				warningMessage "Boot provision failed!"
		void $ async $ simpleSh $ namedPipe cid
		forever $ do
			void $ ifM (inPath "bash")
				( boolSystem "bash" [Param "-l"]
				, boolSystem "/bin/sh" []
				)
			putStrLn "Container is still running. Press ^P^Q to detach."

-- | Once a container is running, propellor can be run inside
-- it to provision it.
--
-- Note that there is a race here, between the simplesh
-- server starting up in the container, and this property
-- being run. So, retry connections to the client for up to
-- 1 minute.
provisionContainer :: ContainerId -> Property
provisionContainer cid = containerDesc cid $ Property "provision" $ do
	r <- simpleShClientRetry 60 (namedPipe cid) "./propellor" params (go Nothing)
	when (r /= FailedChange) $
		setProvisionedFlag cid 
	return r
  where
	params = ["--continue", show $ Chain $ fromContainerId cid]

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
		Done _ -> ret lastline
	go lastline [] = ret lastline

	ret lastline = return $ fromMaybe FailedChange $
		readish =<< lastline

stopContainer :: ContainerId -> IO Bool
stopContainer cid = boolSystem dockercmd [Param "stop", Param $ fromContainerId cid ]

stoppedContainer :: ContainerId -> Property
stoppedContainer cid = containerDesc cid $ Property desc $ 
	ifM (elem cid <$> listContainers RunningContainers)
		( ensureProperty $ boolProperty desc $ stopContainer cid
		, return NoChange
		)
  where
	desc = "stopped"

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
	catMaybes . map toContainerId . catMaybes . map (lastMaybe . words) . lines
		<$> readProcess dockercmd ps
  where
	ps
		| status == AllContainers = baseps ++ ["--all"]
		| otherwise = baseps
	baseps = ["ps", "--no-trunc"]

listImages :: IO [Image]
listImages = lines <$> readProcess dockercmd ["images", "--all", "--quiet"]

runProp :: String -> RunParam -> Containerized Property
runProp field val = 
	Containerized ["--" ++ param] (Property (param) (return NoChange))
  where
	param = field++"="++val

-- | The ContainerIdent of a container is written to
-- /.propellor-ident inside it. This can be checked to see if
-- the container has the same ident later.
propellorIdent :: FilePath
propellorIdent = "/.propellor-ident"

-- | Named pipe used for communication with the container.
namedPipe :: ContainerId -> FilePath
namedPipe cid = "docker/" ++ fromContainerId cid

provisionedFlag :: ContainerId -> FilePath
provisionedFlag cid = "docker/" ++ fromContainerId cid ++ ".provisioned"

clearProvisionedFlag :: ContainerId -> IO ()
clearProvisionedFlag = nukeFile . provisionedFlag

setProvisionedFlag :: ContainerId -> IO ()
setProvisionedFlag cid = do
	createDirectoryIfMissing True (takeDirectory (provisionedFlag cid))
	writeFile (provisionedFlag cid) "1"

checkProvisionedFlag :: ContainerId -> IO Bool
checkProvisionedFlag = doesFileExist . provisionedFlag

identFile :: ContainerId -> FilePath
identFile cid = "docker/" ++ fromContainerId cid ++ ".ident"

readIdentFile :: ContainerId -> IO ContainerIdent
readIdentFile cid = fromMaybe (error "bad ident in identFile")
	. readish <$> readFile (identFile cid)

dockercmd :: String
dockercmd = "docker.io"

report :: [Bool] -> Result
report rmed
	| or rmed = MadeChange
	| otherwise = NoChange


{-# LANGUAGE RankNTypes #-}

module Propellor.Property.Docker where

import Propellor
import Propellor.SimpleSh
import qualified Propellor.Property.File as File
import qualified Propellor.Property.Apt as Apt
import Utility.SafeCommand
import Utility.Path

import Control.Concurrent.Async

dockercmd :: String
dockercmd = "docker.io"

-- | Configures docker with an authentication file, so that images can be
-- pushed to index.docker.io.
configured :: Property
configured = Property "docker configured" go `requires` installed
  where
	go = withPrivData DockerAuthentication $ \cfg -> ensureProperty $ 
		"/root/.dockercfg" `File.hasContent` (lines cfg)

installed :: Property
installed = Apt.installed ["docker.io"]

-- | Parameters to pass to `docker run` when creating a container.
type RunParam = String

data Containerized a = Containerized [RunParam] a

getRunParams :: [Containerized a] -> [RunParam]
getRunParams l = concatMap get l
  where
	get (Containerized ps _) = ps

fromContainerized :: forall a. [Containerized a] -> [a]
fromContainerized l = map get l
  where
	get (Containerized _ a) = a

-- | A docker image, that can be used to run a container.
type Image = String

-- | A short descriptive name for a container.
-- Should not contain whitespace or other unusual characters,
-- only [a-zA-Z0-9_.-] are allowed
type ContainerName = String

-- | A container is identified by its name, and the host
-- on which it's deployed.
data ContainerId = ContainerId HostName ContainerName
	deriving (Eq)

toContainerId :: String -> Maybe ContainerId
toContainerId s = case separate (== '.') s of
	(cn, hn)
		| null hn || null cn -> Nothing
		| otherwise -> Just $ ContainerId hn cn

fromContainerId :: ContainerId -> String
fromContainerId (ContainerId hn cn) = cn++"."++hn

data Container = Container Image [Containerized Property]

containerFrom :: Image -> [Containerized Property] -> Container
containerFrom = Container

containerProperties
	:: (HostName -> ContainerName -> Maybe (Container))
	-> (HostName -> Maybe [Property])
containerProperties findcontainer = \h -> case toContainerId h of
	Nothing -> Nothing
	Just (ContainerId hn cn) ->
		case findcontainer hn cn of
			Nothing -> Nothing
			Just (Container _ cprops) -> 
				Just $ fromContainerized cprops

containerDesc :: ContainerId -> Desc -> Desc
containerDesc cid d = "docker container " ++ fromContainerId cid ++ " " ++ d

-- | Ensures that a docker container is set up and running. The container
-- has its own Properties which are handled by running propellor
-- inside the container.
hasContainer
	:: HostName
	-> ContainerName
	-> (HostName -> ContainerName -> Maybe (Container))
	-> Property
hasContainer hn cn findcontainer = 
	case findcontainer hn cn of
		Nothing -> Property (containerDesc cid "") $ do
			warningMessage $ "missing definition for docker container \"" ++ fromContainerId cid
			return FailedChange
		Just (Container image containerprops) ->
			provisionContainer cid
				`requires`
			runningContainer cid image containerprops
  where
  	cid = ContainerId hn cn

runningContainer :: ContainerId -> Image -> [Containerized Property] -> Property
runningContainer cid@(ContainerId hn cn) image containerprops = Property (containerDesc cid "running") $ do
	l <- listContainers RunningContainers
	if cid `elem` l
		then do
			runningident <- getrunningident
			if runningident == Just ident
				then return NoChange
				else do
					void $ stopContainer cid
					oldimage <- fromMaybe image <$> commitContainer cid
					removeContainer cid
					go oldimage
		else do
			whenM (elem cid <$> listContainers AllContainers) $
				removeContainer cid
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
		-- cd to propellor directory
		, workdir localdir
		]
	
	chaincmd = ["./propellor", "--continue", show $ ChainDocker $ show ident]

	go img = ifM (runContainer img (runps ++ ["-i", "-d", "-t"]) chaincmd)
		( return MadeChange
		, return FailedChange
		)

-- | Two containers with the same ContainerIdent were started from
-- the same base image (possibly a different version though), and
-- with the same RunParams.
data ContainerIdent = ContainerIdent Image HostName ContainerName [RunParam]
	deriving (Read, Show, Eq)

-- | The ContainerIdent of a container is written to
-- /.propellor-ident inside it. This can be checked to see if
-- the container has the same ident later.
propellorIdent :: FilePath
propellorIdent = "/.propellor-ident"

-- | Named pipe used for communication with the container.
namedPipe :: ContainerId -> FilePath
namedPipe cid = "docker/" ++ fromContainerId cid

-- | Called when propellor is running inside a docker container.
-- The string should be the container's ContainerIdent.
--
-- Fork a thread to run the SimpleSh server in the background.
-- In the foreground, run an interactive bash (or sh) shell,
-- so that the user can interact with it when attached to the container.
chain :: String -> IO ()
chain s = case readish s of
	Nothing -> error $ "Invalid ContainerId: " ++ s
	Just ident@(ContainerIdent _image hn cn _rp) -> do
		let cid = ContainerId hn cn
		writeFile propellorIdent (show ident)
		t <- async $ simpleSh $ namedPipe cid
		void $ ifM (inPath "bash")
			( boolSystem "bash" [Param "-l"]
			, boolSystem "/bin/sh" []
			)
		wait t

-- | Once a container is running, propellor can be run inside
-- it to provision it.
--
-- Note that there is a race here, between the simplesh
-- server starting up in the container, and this property
-- being run. So, retry connections to the client for up to
-- 1 minute.
provisionContainer :: ContainerId -> Property
provisionContainer cid = Property (containerDesc cid "provision") $
	simpleShClientRetry 60 (namedPipe cid) "./propellor" [show params] (go Nothing)
  where
	params = Chain $ fromContainerId cid

	go lastline (v:rest) = case v of
		StdoutLine s -> do
			maybe noop putStrLn lastline
			hFlush stdout
			go (Just s) rest
		StderrLine s -> do
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

removeContainer :: ContainerId -> IO ()
removeContainer cid = void $ catchMaybeIO $
	readProcess dockercmd ["rm", fromContainerId cid ]

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

runProp :: String -> RunParam -> Containerized Property
runProp field val = 
	Containerized ["--" ++ param] (Property param (return NoChange))
  where
	param = field++"="++val

-- | Lift a Property to run inside the container.
inside1 :: Property -> Containerized Property
inside1 = Containerized []

inside :: [Property] -> Containerized Property
inside = Containerized [] . combineProperties

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
-- Must contain ./propellor! (Normally set automatically.)
workdir :: String -> Containerized Property
workdir = runProp "workdir"

-- | Memory limit for container.
--Format: <number><optional unit>, where unit = b, k, m or g
memory :: String -> Containerized Property
memory = runProp "memory"

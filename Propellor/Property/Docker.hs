{-# LANGUAGE RankNTypes #-}

module Propellor.Property.Docker where

import Propellor
import Propellor.CmdLine
import Propellor.SimpleSh
import qualified Propellor.Property.File as File
import qualified Propellor.Property.Apt as Apt
import Utility.SafeCommand

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
	deriving (Read, Show, Eq)

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
		Nothing -> Property desc $ do
			warningMessage $ "missing definition for docker container \"" ++ fromContainerId cid
			return FailedChange
		Just (Container image containerprops) ->
			Property desc (provisionContainer cid)
			`requires`
			Property desc (ensureContainer cid image containerprops)
  where
  	cid = ContainerId hn cn

  	desc = "docker container " ++ fromContainerId cid

ensureContainer :: ContainerId -> Image -> [Containerized Property] -> IO Result
ensureContainer cid image containerprops = do
	l <- listContainers Running
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
			whenM (elem cid <$> listContainers Stopped) $
				removeContainer cid
			go image
  where
	ident = ContainerIdent image cid runps

	-- Start the simplesh server that will be used by propellor
	-- to run commands in the container. An interactive shell
	-- is also started, so the user can attach and use it if desired.
	startsimplesh = ["sh" ++  "-c" ++ "./propellor --simplesh " ++ namedPipe cid ++  " & ; bash -l"]

	getrunningident = simpleShClient (namedPipe cid) "cat" [propellorIdent] $
		pure . headMaybe . catMaybes . map readish . catMaybes . map getStdout
	setrunningident = simpleShClient (namedPipe cid) "sh"
		["-c", "echo '" ++ show ident ++ "' > " ++ propellorIdent]
		(const noop)

	runps = getRunParams $ containerprops ++
		-- expose propellor directory inside the container
		[ volume (localdir++":"++localdir)
		-- name the container in a predictable way so we
		-- and the user can easily find it later
		, name (fromContainerId cid)
		-- cd to propellor directory
		, workdir localdir
		]
	
	go img = ifM (runContainer img (runps ++ ["-i", "-d", "-t"]) startsimplesh)
		( do
			setrunningident
			return MadeChange
		, return FailedChange
		)

provisionContainer :: ContainerId -> IO Result
provisionContainer cid = do
	simpleShClient (namedPipe cid) "./propellor" [show params] (go Nothing)
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

-- | Two containers with the same ContainerIdent were started from
-- the same base image (possibly a different version though), and
-- with the same RunParams.
data ContainerIdent = ContainerIdent Image ContainerId [RunParam]
	deriving (Read, Show, Eq)

-- | The ContainerIdent of a container is written to
-- /.propellor-ident inside it. This can be checked to see if
-- the container has the same ident later.
propellorIdent :: FilePath
propellorIdent = "/.propellor-ident"

-- | Named pipe used for communication with the container.
namedPipe :: ContainerId -> FilePath
namedPipe cid = "docker/" ++ fromContainerId cid

stopContainer :: ContainerId -> IO Bool
stopContainer cid = boolSystem dockercmd [Param "stop", Param $ fromContainerId cid ]

removeContainer :: ContainerId -> IO ()
removeContainer cid = void $ boolSystem "sh"
	[Param "-c", Param $ dockercmd ++ " rm " ++ fromContainerId cid ]

runContainer :: Image -> [RunParam] -> [String] -> IO Bool
runContainer image ps cmd = boolSystem dockercmd $ map Param $
	"run" : (ps ++ image : cmd)

commitContainer :: ContainerId -> IO (Maybe Image)
commitContainer cid = catchMaybeIO $
	takeWhile (/= '\n') 
		<$> readProcess dockercmd ["commit", fromContainerId cid]

data ContainerStatus = Running | Stopped
	deriving (Eq)

-- | Only lists propellor managed containers.
listContainers :: ContainerStatus -> IO [ContainerId]
listContainers status = 
	catMaybes . map readish . catMaybes . map (lastMaybe . words) . lines
		<$> readProcess dockercmd ps
  where
	ps
		| status == Stopped = baseps ++ ["--all"]
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

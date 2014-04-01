{-# LANGUAGE RankNTypes #-}

module Propellor.Property.Docker where

import Propellor
import Propellor.CmdLine
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

type ContainerName = String

-- | A container is identified by its name, and the host
-- on which it's deployed.
data ContainerId = ContainerId HostName ContainerName
	deriving (Read, Show, Eq)

toContainerId :: String -> Maybe ContainerId
toContainerId s = case separate (== '@') s of
	(cn, hn)
		| null hn || null cn -> Nothing
		| otherwise -> Just $ ContainerId hn cn

fromContainerId :: ContainerId -> String
fromContainerId (ContainerId hn cn) = cn++"@"++hn

data Container = Container Image [Containerized Property]

containerFromImage :: Image -> [Containerized Property] -> Container
containerFromImage = Container

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
			running image containerprops
  where
  	cid = ContainerId hn cn

  	desc = "docker container " ++ fromContainerId cid

	-- Start the container, if it's not already running.
	running image containerprops = Property desc $ do
		let runps = getRunParams $ containerprops ++
			-- expose propellor directory inside the container
			[ volume (localdir++":"++localdir)
			-- name the container in a predictable way so we
			-- and the user can easily find it later
			, name (fromContainerId cid)
			-- cd to propellor directory
			, workdir localdir
			]
		let ident = ContainerIdent image cid runps
		let runit img = ifM (runContainer cid img runps ident)
			( do
				r <- runinside
				return $ MadeChange <> r
			, return FailedChange
			)
		l <- listRunningContainers
		if cid `elem` l
			then do
				runningident <- readish <$> readContainerCommand cid "cat" ["/.propeller-ident"]
				if runningident == Just ident
					then runinside
					else do
						void $ stopContainer cid
						oldimage <- fromMaybe image <$> commitContainer cid
						removeContainer cid
						runit oldimage
			else do
				removeContainer cid
				runit image

	-- Use propellor binary exposed inside the container
	-- (assumes libc compatablity), and run it, passing it the
	-- container@hostname so it knows what to do.
	-- Read its Result code and propigate
	runinside :: IO Result
	runinside = fromMaybe FailedChange . readish
		<$> readContainerCommand cid "./propellor" [show params]
	  where
	  	-- Using Continue avoids auto-update of the binary inside
		-- the container.
		params = Continue $ Run $ fromContainerId cid

-- | Two containers with the same ContainerIdent were started from
-- the same base image (possibly a different version though), and
-- with the same RunParams.
data ContainerIdent = ContainerIdent Image ContainerId [RunParam]
	deriving (Read, Show, Eq)

-- | The ContainerIdent of a container is written to
-- /.propeller-ident inside it. This can be checked to see if
-- the container has the same ident later.
propellerIdent :: FilePath
propellerIdent = "/.propeller-ident"

stopContainer :: ContainerId -> IO Bool
stopContainer cid = boolSystem dockercmd [Param "stop", Param $ fromContainerId cid ]

removeContainer :: ContainerId -> IO ()
removeContainer cid = void $ boolSystem "sh"
	[Param "-c", Param $ dockercmd ++ " rm " ++ fromContainerId cid ]

runContainer :: ContainerId -> Image -> [RunParam] -> ContainerIdent -> IO Bool
runContainer cid image ps ident = do
	ok <- boolSystem dockercmd undefined
	when ok $
		void $ readContainerCommand cid "sh"
			["-c", "echo '" ++ show ident ++ "' > " ++ propellerIdent]
	return ok

-- | Runs a command inside the container.
readContainerCommand :: ContainerId -> String -> [String] -> IO String
readContainerCommand cid command params = undefined

commitContainer :: ContainerId -> IO (Maybe Image)
commitContainer cid = catchMaybeIO $
	readProcess dockercmd ["commit", fromContainerId cid]

-- | Only lists propellor managed containers.
listRunningContainers :: IO [ContainerId]
listRunningContainers = undefined -- docker.io ps

-- | Only lists propellor managed containers.
listContainers :: IO [ContainerId]
listContainers = undefined

listImages :: IO [ContainerId]
listImages = undefined -- docker.io images --no-trunc

runProp :: String -> RunParam -> Containerized Property
runProp field val = Containerized [param] (Property param (return NoChange))
  where
	param = field++"="++val

-- | Lift a Property to run inside the container.
inside :: Property -> Containerized Property
inside p = Containerized [] p

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

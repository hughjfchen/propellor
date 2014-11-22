module Propellor.Ssh where

import Propellor
import Utility.SafeCommand
import Utility.UserInfo

import System.PosixCompat
import Data.Time.Clock.POSIX

-- Parameters can be passed to both ssh and scp, to enable a ssh connection
-- caching socket.
--
-- If the socket already exists, check if its mtime is older than 10
-- minutes, and if so stop that ssh process, in order to not try to
-- use an old stale connection. (atime would be nicer, but there's
-- a good chance a laptop uses noatime)
sshCachingParams :: HostName -> Bool -> IO [CommandParam]
sshCachingParams hn viarelay = do
	home <- myHomeDir
	let cachedir = home </> ".ssh" </> "propellor"
	createDirectoryIfMissing False cachedir
	let socketfile = cachedir </> hn ++ ".sock"
	let ps = catMaybes
		[ if viarelay then Just (Param "-A") else Nothing
		, Just $ Param "-o"
		, Just $ Param ("ControlPath=" ++ socketfile)
		, Just $ Params "-o ControlMaster=auto -o ControlPersist=yes"
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

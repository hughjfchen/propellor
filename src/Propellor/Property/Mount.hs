module Propellor.Property.Mount where

import Propellor.Base
import Utility.Path

type FsType = String -- ^ type of filesystem to mount ("auto" to autodetect)

type Source = String

-- | Lists all mount points of the system.
mountPoints :: IO [FilePath]
mountPoints = lines <$> readProcess "findmnt" ["-rn", "--output", "target"]

-- | Finds all filesystems mounted inside the specified directory.
mountPointsBelow :: FilePath -> IO [FilePath]
mountPointsBelow target = filter (\p -> simplifyPath p /= simplifyPath target)
	. filter (dirContains target)
	<$> mountPoints

-- | Filesystem type mounted at a given location.
getFsType :: FilePath -> IO (Maybe FsType)
getFsType mnt = catchDefaultIO Nothing $
	headMaybe . lines
		<$> readProcess "findmnt" ["-n", mnt, "--output", "fstype"]

-- | Unmounts a device, lazily so any running processes don't block it.
umountLazy :: FilePath -> IO ()
umountLazy mnt =  
	unlessM (boolSystem "umount" [ Param "-l", Param mnt ]) $
		errorMessage $ "failed unmounting " ++ mnt

-- | Unmounts anything mounted inside the specified directory.
unmountBelow :: FilePath -> IO ()
unmountBelow d = do
	submnts <- mountPointsBelow d
	forM_ submnts umountLazy

-- | Mounts a device.
mount :: FsType -> Source -> FilePath -> IO Bool
mount fs src mnt = boolSystem "mount" [Param "-t", Param fs, Param src, Param mnt]

module Propellor.Property.Mount where

import Propellor
import Utility.SafeCommand

type FsType = String
type Source = String

mountPoints :: IO [FilePath]
mountPoints = lines <$> readProcess "findmnt" ["-rn", "--output", "target"]

getFsType :: FilePath -> IO (Maybe FsType)
getFsType mnt = catchDefaultIO Nothing $
	headMaybe . lines
		<$> readProcess "findmnt" ["-n", mnt, "--output", "fstype"]

umountLazy :: FilePath -> IO ()
umountLazy mnt =  
	unlessM (boolSystem "umount" [ Param "-l", Param mnt ]) $
		errorMessage $ "failed unmounting " ++ mnt

mount :: FsType -> Source -> FilePath -> IO Bool
mount fs src mnt = boolSystem "mount" [Param "-t", Param fs, Param src, Param mnt]

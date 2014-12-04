module Propellor.Property.Mount where

import Propellor
import Utility.SafeCommand

mountPoints :: IO [FilePath]
mountPoints = lines <$> readProcess "findmnt" ["-rn", "--output", "target"]

umountLazy :: FilePath -> IO ()
umountLazy mnt =  
	unlessM (boolSystem "umount" [ Param "-l", Param mnt ]) $
		errorMessage $ "failed unmounting " ++ mnt

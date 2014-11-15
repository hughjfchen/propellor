module Propellor.PrivData.Paths where

import System.FilePath

privDataDir :: FilePath
privDataDir = "privdata.joey"

privDataFile :: FilePath
privDataFile = privDataDir </> "privdata.gpg"

privDataLocal :: FilePath
privDataLocal = privDataDir </> "local"

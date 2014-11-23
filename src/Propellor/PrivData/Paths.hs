module Propellor.PrivData.Paths where

import System.FilePath

privDataDir :: FilePath
privDataDir = "privdata"

privDataFile :: FilePath
privDataFile = privDataDir </> "privdata.gpg"

privDataLocal :: FilePath
privDataLocal = privDataDir </> "local"

privDataRelay :: String -> FilePath
privDataRelay host = privDataDir </> "relay" </> host

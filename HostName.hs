module HostName where

import Data.Maybe
import Control.Applicative
import System.Environment

import qualified Utility.Network as Network

type HostName = String

getHostName :: IO HostName
getHostName = go =<< getArgs
  where
	go (h:_) = return h
	go [] = fromMaybe nohostname <$> Network.getHostname
	nohostname = error "Cannot determine hostname! Pass it on the command line."

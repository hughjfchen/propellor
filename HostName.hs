module HostName where

import Control.Applicative
import System.Environment

import Utility.Process

type HostName = String

getHostName :: IO HostName
getHostName = go =<< getArgs
  where
	go (h:_) = return h
	go [] = do
		s <- takeWhile (/= '\n') <$> readProcess "hostname" ["-f"]
		if null s
			then error "Cannot determine hostname! Pass it on the command line."
			else return s

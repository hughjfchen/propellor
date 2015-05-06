{-# LANGUAGE PackageImports #-}

module Propellor.Exception where

import Propellor.Types
import Propellor.Message
import Utility.Exception

import Control.Exception (IOException)

-- | Catches IO exceptions and returns FailedChange.
catchPropellor :: Propellor Result -> Propellor Result
catchPropellor a = either err return =<< tryPropellor a
  where
	err e =  warningMessage (show e) >> return FailedChange

tryPropellor :: Propellor a -> Propellor (Either IOException a)
tryPropellor = try

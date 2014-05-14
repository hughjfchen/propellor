{-# LANGUAGE PackageImports #-}

module Propellor.Exception where

import qualified "MonadCatchIO-transformers" Control.Monad.CatchIO as M
import Control.Exception

import Propellor.Types
import Propellor.Message

-- | Catches IO exceptions and returns FailedChange.
catchPropellor :: Propellor Result -> Propellor Result
catchPropellor a = either err return =<< tryPropellor a
  where
	err e =  warningMessage (show e) >> return FailedChange

tryPropellor :: Propellor a -> Propellor (Either IOException a)
tryPropellor = M.try

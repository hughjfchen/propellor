{-# LANGUAGE PackageImports #-}

module Propellor.Exception where

import qualified "MonadCatchIO-transformers" Control.Monad.CatchIO as M
import Control.Exception
import Control.Applicative

import Propellor.Types

-- | Catches IO exceptions and returns FailedChange.
catchPropellor :: Propellor Result -> Propellor Result
catchPropellor a = either (\_ -> FailedChange) id <$> tryPropellor a

tryPropellor :: Propellor a -> Propellor (Either IOException a)
tryPropellor = M.try

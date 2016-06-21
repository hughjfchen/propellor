{-# LANGUAGE ScopedTypeVariables #-}

module Propellor.Exception where

import Propellor.Types
import Propellor.Types.Exception
import Propellor.Message
import Utility.Exception

import Control.Exception (AsyncException)
import Control.Monad.Catch
import Control.Monad.IO.Class (MonadIO)

-- | Catches all exceptions (except for `StopPropellorException` and
-- `AsyncException`) and returns FailedChange.
catchPropellor :: (MonadIO m, MonadCatch m) => m Result -> m Result
catchPropellor a = either err return =<< tryPropellor a
  where
	err e =  warningMessage (show e) >> return FailedChange

catchPropellor' :: MonadCatch m => m a -> (SomeException -> m a) -> m a
catchPropellor' a onerr = a `catches`
	[ Handler (\ (e :: AsyncException) -> throwM e)
	, Handler (\ (e :: StopPropellorException) -> throwM e)
	, Handler (\ (e :: SomeException) -> onerr e)
	]

-- | Catches all exceptions (except for `StopPropellorException` and
-- `AsyncException`).
tryPropellor :: MonadCatch m => m a -> m (Either SomeException a)
tryPropellor a = (Right <$> a) `catchPropellor'` (pure . Left)

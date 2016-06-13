{-# LANGUAGE ScopedTypeVariables #-}

module Propellor.Exception where

import Propellor.Types
import Propellor.Message
import Utility.Exception

import Control.Exception (AsyncException)
import Control.Monad.Catch
import Control.Monad.IO.Class (MonadIO)
import Data.Typeable

-- | Normally when an exception is encountered while propellor is 
-- ensuring a property, the property fails, but propellor robustly 
-- continues on to the next property.
--
-- This is the only exception that will stop the entire propellor run,
-- preventing any subsequent properties of the Host from being ensured.
-- (When propellor is running in a container in a Host, this exception only
-- stops the propellor run in the container; the outer run in the Host
-- continues.)
--
-- You should only throw this exception when things are so badly messed up
-- that it's best for propellor to not try to do anything else.
data StopPropellorException = StopPropellorException String
	deriving (Show, Typeable)

instance Exception StopPropellorException

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

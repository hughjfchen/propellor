{-# LANGUAGE FlexibleContexts #-}

-- | Note that this module does not yet arrange for any output multiplexing,
-- so the output of concurrent properties will be scrambled together.

module Propellor.Property.Concurrent (
	concurrently,
	concurrentList,
	props,
	getNumProcessors,
	withCapabilities,
	concurrentSatisfy,
) where

import Propellor.Base

import Control.Concurrent
import qualified Control.Concurrent.Async as A
import GHC.Conc (getNumProcessors)
import Control.Monad.RWS.Strict

-- | Ensures two properties concurrently.
concurrently
	:: (IsProp (Property x), IsProp (Property y), Combines (Property x) (Property y), IsProp (Property (CInfo x y)))
	=> Property x
	-> Property y
	-> CombinedType (Property x) (Property y)
concurrently p1 p2 = (combineWith go p1 p2)
	`describe` d
  where
	d = getDesc p1 ++ " `concurrently` " ++ getDesc p2
	-- Increase the number of capabilities right up to the number of
	-- processors, so that A `concurrently` B `concurrently` C
	-- runs all 3 properties on different processors when possible.
	go a1 a2 = do
		n <- liftIO getNumProcessors
		withCapabilities n $
			concurrentSatisfy a1 a2

-- | Ensures all the properties in the list, with a specified amount of
-- concurrency.
-- 
-- > concurrentList (pure 2) "demo" $ props
-- >	& foo
-- >	& bar
-- >	& baz
--
-- The above example will run foo and bar concurrently, and once either of
-- those 2 properties finishes, will start running baz.
concurrentList :: IO Int -> Desc -> PropList -> Property HasInfo
concurrentList getn d (PropList ps) = infoProperty d go mempty ps
  where
	go = do
		n <- liftIO getn
		withCapabilities n $
			startworkers n =<< liftIO (newMVar ps)
	startworkers n q
		| n < 1 = return NoChange
		| n == 1 = worker q NoChange
		| otherwise = 
			worker q NoChange
				`concurrentSatisfy`
			startworkers (n-1) q
	worker q r = do
		v <- liftIO $ modifyMVar q $ \v -> case v of
			[] -> return ([], Nothing)
			(p:rest) -> return (rest, Just p)
		case v of
			Nothing -> return r
			-- This use of propertySatisfy does not lose any
			-- Info asociated with the property, because
			-- concurrentList sets all the properties as
			-- children, and so propigates their info.
			Just p -> do
				hn <- asks hostName
				r' <- actionMessageOn hn
					(propertyDesc p)
					(propertySatisfy p)
				worker q (r <> r')

-- | Run an action with the number of capabiities increased as necessary to
-- allow running on the specified number of cores.
--
-- Never increases the number of capabilities higher than the actual number
-- of processors.
withCapabilities :: Int -> Propellor a -> Propellor a
withCapabilities n a = bracket setup cleanup (const a)
  where
	setup = do
		np <- liftIO getNumProcessors
		let n' = min n np
		c <- liftIO getNumCapabilities
		when (n' > c) $ 
			liftIO $ setNumCapabilities n'
		return c
	cleanup = liftIO . setNumCapabilities

concurrentSatisfy :: Propellor Result -> Propellor Result -> Propellor Result
concurrentSatisfy a1 a2 = do
	h <- ask
	((r1, w1), (r2, w2)) <- liftIO $
		runp a1 h `A.concurrently` runp a2 h
	tell (w1 <> w2)
	return (r1 <> r2)
  where
	runp a h = evalRWST (runWithHost (catchPropellor a)) h ()

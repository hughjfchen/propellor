{-# LANGUAGE PackageImports #-}

module Propellor.Engine where

import System.Exit
import System.IO
import Data.Monoid
import Control.Applicative
import System.Console.ANSI
import "mtl" Control.Monad.Reader

import Propellor.Types
import Propellor.Message
import Propellor.Exception
import Propellor.Info

runPropellor :: Host -> Propellor a -> IO a
runPropellor host a = runReaderT (runWithHost a) host

mainProperties :: Host -> IO ()
mainProperties host = do
	r <- runPropellor host $
		ensureProperties [Property "overall" (ensureProperties $ hostProperties host) mempty]
	setTitle "propellor: done"
	hFlush stdout
	case r of
		FailedChange -> exitWith (ExitFailure 1)
		_ -> exitWith ExitSuccess

ensureProperties :: [Property] -> Propellor Result
ensureProperties ps = ensure ps NoChange
  where
	ensure [] rs = return rs
	ensure (l:ls) rs = do
		hn <- asks hostName
		r <- actionMessageOn hn (propertyDesc l) (ensureProperty l)
		ensure ls (r <> rs)

ensureProperty :: Property -> Propellor Result
ensureProperty = catchPropellor . propertySatisfy

-- | Lifts an action into a different host.
--
-- For example, `fromHost hosts "otherhost" getSshPubKey`
fromHost :: [Host] -> HostName -> Propellor a -> Propellor (Maybe a)
fromHost l hn getter = case findHost l hn of
	Nothing -> return Nothing
	Just h -> liftIO $ Just <$>
		runReaderT (runWithHost getter) h

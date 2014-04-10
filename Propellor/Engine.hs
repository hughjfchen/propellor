{-# LANGUAGE PackageImports #-}

module Propellor.Engine where

import System.Exit
import System.IO
import Data.Monoid
import System.Console.ANSI
import "mtl" Control.Monad.Reader

import Propellor.Types
import Propellor.Message
import Propellor.Exception

runPropellor :: HostAttr -> Propellor a -> IO a
runPropellor hostattr a = runReaderT (runWithHostAttr a) hostattr

mainProperties :: HostAttr -> [Property] -> IO ()
mainProperties hostattr ps = do
	r <- runPropellor hostattr $
		ensureProperties [Property "overall" $ ensureProperties ps]
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
		r <- actionMessage (propertyDesc l) (ensureProperty l)
		ensure ls (r <> rs)

ensureProperty :: Property -> Propellor Result
ensureProperty = catchPropellor . propertySatisfy

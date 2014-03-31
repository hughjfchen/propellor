module Propellor.Engine where

import System.Exit
import System.IO
import Data.Monoid
import System.Console.ANSI

import Propellor.Types
import Propellor.Message
import Utility.Exception

ensureProperty :: Property -> IO Result
ensureProperty = catchDefaultIO FailedChange . propertySatisfy

ensureProperties :: [Property] -> IO ()
ensureProperties ps = do
	r <- ensureProperties' [Property "overall" $ ensureProperties' ps]
	setTitle "propellor: done"
	hFlush stdout
	case r of
		FailedChange -> exitWith (ExitFailure 1)
		_ -> exitWith ExitSuccess

ensureProperties' :: [Property] -> IO Result
ensureProperties' ps = ensure ps NoChange
  where
	ensure [] rs = return rs
	ensure (l:ls) rs = do
		r <- actionMessage (propertyDesc l) (ensureProperty l)
		ensure ls (r <> rs)

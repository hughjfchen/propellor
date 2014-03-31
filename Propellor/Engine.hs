module Propellor.Engine where

import System.Console.ANSI
import System.Exit
import System.IO

import Propellor.Types
import Utility.Exception

ensureProperty :: Property -> IO Result
ensureProperty = catchDefaultIO FailedChange . propertySatisfy

ensureProperties :: [Property] -> IO ()
ensureProperties ps = do
	r <- ensureProperties' [Property "overall" $ ensureProperties' ps]
	setTitle "propellor done"
	hFlush stdout
	case r of
		FailedChange -> exitWith (ExitFailure 1)
		_ -> exitWith ExitSuccess

ensureProperties' :: [Property] -> IO Result
ensureProperties' ps = ensure ps NoChange
  where
	ensure [] rs = return rs
	ensure (l:ls) rs = do
		setTitle $ propertyDesc l
		hFlush stdout
		r <- ensureProperty l
		clearFromCursorToLineBeginning
		setCursorColumn 0
		putStr $ propertyDesc l ++ "... "
		case r of
			FailedChange -> do
				setSGR [SetColor Foreground Vivid Red]
				putStrLn "failed"
			NoChange -> do
				setSGR [SetColor Foreground Dull Green]
				putStrLn "unchanged"
			MadeChange -> do
				setSGR [SetColor Foreground Vivid Green]
				putStrLn "done"
		setSGR []
		hFlush stdout
		ensure ls (combineResult r rs)

warningMessage :: String -> IO ()
warningMessage s = do
	setSGR [SetColor Foreground Vivid Red]
	putStrLn $ "** warning: " ++ s
	setSGR []
	hFlush stdout

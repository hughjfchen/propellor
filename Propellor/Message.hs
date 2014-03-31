module Propellor.Message where

import System.Console.ANSI
import System.IO

import Propellor.Types

-- | Shows a message while performing an action, with a colored status
-- display.
actionMessage :: ActionResult r => Desc -> IO r -> IO r
actionMessage desc a = do
	setTitle $ "propellor: " ++ desc
	hFlush stdout

	r <- a

	let (msg, intensity, color) = getActionResult r
	putStr $ desc ++ " ... "
	setSGR [SetColor Foreground intensity color]
	putStrLn msg
	setSGR []
	setTitle "propellor: running"
	hFlush stdout

	return r

warningMessage :: String -> IO ()
warningMessage s = do
	setSGR [SetColor Foreground Vivid Red]
	putStrLn $ "** warning: " ++ s
	setSGR []
	hFlush stdout

errorMessage :: String -> IO a
errorMessage s = do
	warningMessage s
	error "Propellor failed!"

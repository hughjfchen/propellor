module Propellor.Message where

import System.Console.ANSI
import System.IO
import System.Log.Logger

import Propellor.Types

-- | Shows a message while performing an action, with a colored status
-- display.
actionMessage :: ActionResult r => Desc -> IO r -> IO r
actionMessage desc a = do
	setTitle $ "propellor: " ++ desc
	hFlush stdout

	r <- a

	setTitle "propellor: running"
	let (msg, intensity, color) = getActionResult r
	putStr $ desc ++ " ... "
	colorLine intensity color msg
	hFlush stdout

	return r

warningMessage :: String -> IO ()
warningMessage s = colorLine Vivid Red $ "** warning: " ++ s

colorLine :: ColorIntensity -> Color -> String -> IO ()
colorLine intensity color msg = do
	setSGR [SetColor Foreground intensity color]
	putStr msg
	setSGR []
	-- Note this comes after the color is reset, so that
	-- the color set and reset happen in the same line.
	putStrLn ""
	hFlush stdout

errorMessage :: String -> IO a
errorMessage s = do
	warningMessage s
	error "Cannot continue!"

-- | Causes a debug message to be displayed when PROPELLOR_DEBUG=1
debug :: [String] -> IO ()
debug = debugM "propellor" . unwords

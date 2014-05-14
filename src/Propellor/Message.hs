{-# LANGUAGE PackageImports #-}

module Propellor.Message where

import System.Console.ANSI
import System.IO
import System.Log.Logger
import "mtl" Control.Monad.Reader

import Propellor.Types

-- | Shows a message while performing an action, with a colored status
-- display.
actionMessage :: (MonadIO m, ActionResult r) => Desc -> m r -> m r
actionMessage desc a = do
	liftIO $ do
		setTitle $ "propellor: " ++ desc
		hFlush stdout

	r <- a

	liftIO $ do
		setTitle "propellor: running"
		let (msg, intensity, color) = getActionResult r
		putStr $ desc ++ " ... "
		colorLine intensity color msg
		hFlush stdout

	return r

warningMessage :: MonadIO m => String -> m ()
warningMessage s = liftIO $ colorLine Vivid Magenta $ "** warning: " ++ s

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
	liftIO $ colorLine Vivid Red $ "** error: " ++ s
	error "Cannot continue!"

-- | Causes a debug message to be displayed when PROPELLOR_DEBUG=1
debug :: [String] -> IO ()
debug = debugM "propellor" . unwords

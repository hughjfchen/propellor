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
actionMessage = actionMessage' Nothing

-- | Shows a message while performing an action on a specified host,
-- with a colored status display.
actionMessageOn :: (MonadIO m, ActionResult r) => HostName -> Desc -> m r -> m r
actionMessageOn = actionMessage' . Just

actionMessage' :: (MonadIO m, ActionResult r) => Maybe HostName -> Desc -> m r -> m r
actionMessage' mhn desc a = do
	liftIO $ do
		setTitle $ "propellor: " ++ desc
		hFlush stdout

	r <- a

	liftIO $ do
		setTitle "propellor: running"
		showhn mhn
		putStr $ desc ++ " ... "
		let (msg, intensity, color) = getActionResult r
		colorLine intensity color msg
		hFlush stdout

	return r
  where
	showhn Nothing = return ()
	showhn (Just hn) = do
		setSGR [SetColor Foreground Dull Cyan]
		putStr (hn ++ " ")
		setSGR []

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

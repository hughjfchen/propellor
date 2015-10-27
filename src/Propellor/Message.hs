{-# LANGUAGE PackageImports #-}

module Propellor.Message where

import System.Console.ANSI
import System.IO
import System.Log.Logger
import System.Log.Formatter
import System.Log.Handler (setFormatter)
import System.Log.Handler.Simple
import "mtl" Control.Monad.Reader
import Control.Applicative
import System.Directory
import Control.Monad.IfElse
import System.IO.Unsafe (unsafePerformIO)
import Control.Concurrent

import Propellor.Types
import Utility.Monad
import Utility.Env
import Utility.Process
import Utility.Exception

data MessageHandle = MessageHandle
	{ isConsole :: Bool
	}

-- | A shared global variable for the MessageHandle.
{-# NOINLINE globalMessageHandle #-}
globalMessageHandle :: MVar MessageHandle
globalMessageHandle = unsafePerformIO $ do
	c <- hIsTerminalDevice stdout
	newMVar $ MessageHandle c

getMessageHandle :: IO MessageHandle
getMessageHandle = readMVar globalMessageHandle

forceConsole :: IO ()
forceConsole = modifyMVar_ globalMessageHandle $ \mh ->
	pure (mh { isConsole = True })

whenConsole :: IO () -> IO ()
whenConsole a = whenM (isConsole <$> getMessageHandle) a

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
	liftIO $ whenConsole $ do
		setTitle $ "propellor: " ++ desc
		hFlush stdout

	r <- a

	liftIO $ do
		whenConsole $
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
		whenConsole $
			setSGR [SetColor Foreground Dull Cyan]
		putStr (hn ++ " ")
		whenConsole $
			setSGR []

warningMessage :: MonadIO m => String -> m ()
warningMessage s = liftIO $
	colorLine Vivid Magenta $ "** warning: " ++ s

errorMessage :: MonadIO m => String -> m a
errorMessage s = liftIO $ do
	colorLine Vivid Red $ "** error: " ++ s
	error "Cannot continue!"

colorLine :: ColorIntensity -> Color -> String -> IO ()
colorLine intensity color msg = do
	whenConsole $
		setSGR [SetColor Foreground intensity color]
	putStr msg
	whenConsole $
		setSGR []
	-- Note this comes after the color is reset, so that
	-- the color set and reset happen in the same line.
	putStrLn ""
	hFlush stdout

debug :: [String] -> IO ()
debug = debugM "propellor" . unwords

checkDebugMode :: IO ()
checkDebugMode = go =<< getEnv "PROPELLOR_DEBUG"
  where
	go (Just "1") = enableDebugMode
	go (Just _) = noop
	go Nothing = whenM (doesDirectoryExist ".git") $
		whenM (elem "1" . lines <$> getgitconfig) enableDebugMode
	getgitconfig = catchDefaultIO "" $
		readProcess "git" ["config", "propellor.debug"]

enableDebugMode :: IO ()
enableDebugMode = do
	f <- setFormatter
		<$> streamHandler stderr DEBUG
		<*> pure (simpleLogFormatter "[$time] $msg")
	updateGlobalLogger rootLoggerName $ 
		setLevel DEBUG .  setHandlers [f]

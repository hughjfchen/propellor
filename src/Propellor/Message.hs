{-# LANGUAGE PackageImports #-}

-- | This module handles all display of output to the console when
-- propellor is ensuring Properties.
--
-- When two threads both try to display a message concurrently, 
-- the messages will be displayed sequentially.

module Propellor.Message (
	getMessageHandle,
	isConsole,
	forceConsole,
	actionMessage,
	actionMessageOn,
	warningMessage,
	infoMessage,
	errorMessage,
	debug,
	checkDebugMode,
	enableDebugMode,
	processChainOutput,
	messagesDone,
) where

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
import Utility.PartialPrelude
import Utility.Monad
import Utility.Env
import Utility.Process
import Utility.Exception

data MessageHandle = MessageHandle
	{ isConsole :: Bool
	, outputLock :: MVar ()
	}

-- | A shared global variable for the MessageHandle.
{-# NOINLINE globalMessageHandle #-}
globalMessageHandle :: MVar MessageHandle
globalMessageHandle = unsafePerformIO $ do
	c <- hIsTerminalDevice stdout
	o <- newMVar ()
	newMVar $ MessageHandle c o

-- | Gets the global MessageHandle.
getMessageHandle :: IO MessageHandle
getMessageHandle = readMVar globalMessageHandle

-- | Takes a lock while performing an action. Any other threads
-- that try to lockOutput at the same time will block.
lockOutput :: (MonadIO m, MonadMask m) => m a -> m a
lockOutput a = do
	lck <- liftIO $ outputLock <$> getMessageHandle
	bracket_ (liftIO $ takeMVar lck) (liftIO $ putMVar lck ()) a

-- | Force console output. This can be used when stdout is not directly
-- connected to a console, but is eventually going to be displayed at a
-- console.
forceConsole :: IO ()
forceConsole = modifyMVar_ globalMessageHandle $ \mh ->
	pure (mh { isConsole = True })

-- | Only performs the action when at the console, or when console
-- output has been forced.
whenConsole :: IO () -> IO ()
whenConsole a = whenM (isConsole <$> getMessageHandle) a

-- | Shows a message while performing an action, with a colored status
-- display.
actionMessage :: (MonadIO m, MonadMask m, ActionResult r) => Desc -> m r -> m r
actionMessage = actionMessage' Nothing

-- | Shows a message while performing an action on a specified host,
-- with a colored status display.
actionMessageOn :: (MonadIO m, MonadMask m, ActionResult r) => HostName -> Desc -> m r -> m r
actionMessageOn = actionMessage' . Just

actionMessage' :: (MonadIO m, MonadMask m, ActionResult r) => Maybe HostName -> Desc -> m r -> m r
actionMessage' mhn desc a = lockOutput $ do
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
warningMessage s = liftIO $ lockOutput $
	colorLine Vivid Magenta $ "** warning: " ++ s

infoMessage :: MonadIO m => [String] -> m ()
infoMessage ls = liftIO $ lockOutput $
	mapM_ putStrLn ls

errorMessage :: MonadIO m => String -> m a
errorMessage s = liftIO $ lockOutput $ do
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

-- | Reads and displays each line from the Handle, except for the last line
-- which is a Result.
processChainOutput :: Handle -> IO Result
processChainOutput h = go Nothing
  where
	go lastline = do
		v <- catchMaybeIO (hGetLine h)
		debug ["read from chained propellor: ", show v]
		case v of
			Nothing -> case lastline of
				Nothing -> do
					debug ["chained propellor output nothing; assuming it failed"]
					return FailedChange
				Just l -> case readish l of
					Just r -> pure r
					Nothing -> do
						debug ["chained propellor output did not end with a Result; assuming it failed"]
						lockOutput $ do
							putStrLn l
							hFlush stdout
						return FailedChange
			Just s -> do
				lockOutput $ do
					maybe noop (\l -> unless (null l) (putStrLn l)) lastline
					hFlush stdout
				go (Just s)

-- | Called when all messages about properties have been printed.
messagesDone :: IO ()
messagesDone = lockOutput $ do
	whenConsole $
		setTitle "propellor: done"
	hFlush stdout

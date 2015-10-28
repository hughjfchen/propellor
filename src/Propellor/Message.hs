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
	processChainOutput,
	messagesDone,
	createProcessConcurrent,
) where

import System.Console.ANSI
import System.IO
import System.Posix.IO
import "mtl" Control.Monad.Reader
import Control.Applicative
import Control.Monad.IfElse
import System.IO.Unsafe (unsafePerformIO)
import Control.Concurrent
import Control.Concurrent.Async
import Data.Maybe
import Data.Char
import Data.List
import Data.Monoid
import qualified Data.ByteString as B
import qualified System.Process as P

import Propellor.Types
import Utility.PartialPrelude
import Utility.Monad
import Utility.Exception

data MessageHandle = MessageHandle
	{ isConsole :: Bool
	, outputLock :: MVar () -- ^ empty when locked
	, outputLockedBy :: MVar Locker
	}

data Locker
	= GeneralLock
	| ProcessLock P.ProcessHandle

-- | A shared global variable for the MessageHandle.
{-# NOINLINE globalMessageHandle #-}
globalMessageHandle :: MVar MessageHandle
globalMessageHandle = unsafePerformIO $ 
	newMVar =<< MessageHandle
		<$> hIsTerminalDevice stdout
		<*> newMVar ()
		<*> newEmptyMVar

-- | Gets the global MessageHandle.
getMessageHandle :: IO MessageHandle
getMessageHandle = readMVar globalMessageHandle

-- | Takes a lock while performing an action. Any other threads
-- that try to lockOutput at the same time will block.
lockOutput :: (MonadIO m, MonadMask m) => m a -> m a
lockOutput = bracket_ (liftIO takeOutputLock) (liftIO dropOutputLock)

-- | Blocks until we have the output lock.
takeOutputLock :: IO ()
takeOutputLock = void $ takeOutputLock' True

-- | Tries to take the output lock, without blocking.
tryTakeOutputLock :: IO Bool
tryTakeOutputLock = takeOutputLock' False

takeOutputLock' :: Bool -> IO Bool
takeOutputLock' block = do 
	lck <- outputLock <$> getMessageHandle
	go =<< tryTakeMVar lck
  where
	-- lck was full, and we've emptied it, so we hold the lock now.
	go (Just ()) = havelock
	-- lck is empty, so someone else is holding the lock.
	go Nothing = do
		lcker <- outputLockedBy <$> getMessageHandle
		v' <- tryTakeMVar lcker
		case v' of
			Just (ProcessLock h) ->
				-- if process has exited, lock is stale
				ifM (isJust <$> P.getProcessExitCode h)
					( havelock
					, if block
						then do
							void $ P.waitForProcess h
							havelock
						else do
							putMVar lcker (ProcessLock h)
							return False
					)
			Just GeneralLock -> do
				putMVar lcker GeneralLock
				whenblock waitlock
			Nothing -> whenblock waitlock

	havelock = do
		updateOutputLocker GeneralLock
		return True
	waitlock = do
		-- Wait for current lock holder to relinquish
		-- it and take the lock.
		lck <- outputLock <$> getMessageHandle
		takeMVar lck
		havelock
	whenblock a = if block then a else return False

-- | Only safe to call after taking the output lock.
dropOutputLock :: IO ()
dropOutputLock = do
	lcker <- outputLockedBy <$> getMessageHandle
	lck <- outputLock <$> getMessageHandle
	void $ takeMVar lcker
	putMVar lck ()

-- | Only safe to call after takeOutputLock; updates the Locker.
updateOutputLocker :: Locker -> IO ()
updateOutputLocker l = do
	lcker <- outputLockedBy <$> getMessageHandle
	void $ tryTakeMVar lcker
	putMVar lcker l
	modifyMVar_ lcker (const $ return l)

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
actionMessage' mhn desc a = do
	liftIO $ whenConsole $ lockOutput $ do
		setTitle $ "propellor: " ++ desc
		hFlush stdout

	r <- a

	liftIO $ lockOutput $ do
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

-- | Reads and displays each line from the Handle, except for the last line
-- which is a Result.
processChainOutput :: Handle -> IO Result
processChainOutput h = go Nothing
  where
	go lastline = do
		v <- catchMaybeIO (hGetLine h)
		case v of
			Nothing -> case lastline of
				Nothing -> do
					return FailedChange
				Just l -> case readish l of
					Just r -> pure r
					Nothing -> do
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

-- | Wrapper around `System.Process.createProcess` that prevents processes
-- that are running concurrently from writing to the stdout/stderr at the
-- same time.
--
-- The first process run by createProcess is allowed to write to
-- stdout and stderr in the usual way.
--
-- However, if a second createProcess runs concurrently with the
-- first, any stdout or stderr that would have been displayed by it is
-- instead buffered. The buffered output will be displayed the next time it
-- is safe to do so (ie, after the first process exits).
--
-- `Propellor.Property.Cmd` has some other useful actions for running
-- commands, which are based on this.
--
-- Also does debug logging of all commands run.
createProcessConcurrent :: P.CreateProcess -> IO (Maybe Handle, Maybe Handle, Maybe Handle, P.ProcessHandle) 
createProcessConcurrent p
	| hasoutput (P.std_out p) || hasoutput (P.std_err p) =
		ifM tryTakeOutputLock
			( firstprocess
			, concurrentprocess
			)
	| otherwise = P.createProcess p
  where
	hasoutput P.Inherit = True
	hasoutput _ = False

	firstprocess = do
		r@(_, _, _, h) <- P.createProcess p
			`onException` dropOutputLock
		updateOutputLocker (ProcessLock h)
		-- Output lock is still held as we return; the process
		-- is running now, and once it exits the output lock will
		-- be stale and can then be taken by something else.
		return r
	
	concurrentprocess = do
		(toouth, fromouth) <- pipe
		(toerrh, fromerrh) <- pipe
		let p' = p
			{ P.std_out = if hasoutput (P.std_out p)
				then P.UseHandle toouth
				else P.std_out p
			, P.std_err = if hasoutput (P.std_err p)
				then P.UseHandle toerrh
				else P.std_err p
			}
		r <- P.createProcess p'
		hClose toouth
		hClose toerrh
		buf <- newMVar []
		void $ async $ outputDrainer fromouth stdout buf
		void $ async $ outputDrainer fromerrh stderr buf
		void $ async $ bufferWriter buf
		return r

	pipe = do
		(from, to) <- createPipe
		(,) <$> fdToHandle to <*> fdToHandle from

type Buffer = [(Handle, Maybe B.ByteString)]

-- Drain output from the handle, and buffer it in memory.
outputDrainer :: Handle -> Handle -> MVar Buffer -> IO ()
outputDrainer fromh toh buf = do
	v <- tryIO $ B.hGetSome fromh 1024
	case v of
		Right b | not (B.null b) -> do
			modifyMVar_ buf (pure . addBuffer (toh, Just b))
			outputDrainer fromh toh buf
		_ -> do
			modifyMVar_ buf (pure . (++ [(toh, Nothing)]))
			hClose fromh

-- Wait to lock output, and once we can, display everything 
-- that's put into buffer, until the end is signaled by Nothing
-- for both stdout and stderr.
bufferWriter :: MVar Buffer -> IO ()
bufferWriter buf = lockOutput (go [stdout, stderr])
  where
  	go [] = return ()
	go hs = do
		l <- takeMVar buf
		forM_ l $ \(h, mb) -> do
			maybe noop (B.hPut h) mb
			hFlush h
		let hs' = filter (\h -> not (any (== (h, Nothing)) l)) hs
		putMVar buf []
		go hs'
	
-- The buffer can grow up to 1 mb in size, but after that point,
-- it's truncated to avoid propellor using unbounded memory
-- when a process outputs a whole lot of stuff.
bufsz :: Int
bufsz = 1000000

addBuffer :: (Handle, Maybe B.ByteString) -> Buffer -> Buffer
addBuffer v@(_, Nothing) buf = buf ++ [v]
addBuffer (toh, Just b) buf = (toh, Just b') : other
  where
  	(this, other) = partition (\v -> fst v == toh && isJust (snd v)) buf
	b' = truncateBuffer $ B.concat (mapMaybe snd this) <> b
	
-- Truncate a buffer by removing lines from the front until it's
-- small enough.
truncateBuffer :: B.ByteString -> B.ByteString
truncateBuffer b
	| B.length b <= bufsz = b
	| otherwise = truncateBuffer $ snd $ B.breakByte nl b
  where	
	nl = fromIntegral (ord '\n')

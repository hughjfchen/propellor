-- | Concurrent output handling.
--
-- When two threads both try to display a message concurrently, 
-- the messages will be displayed sequentially.

module Utility.ConcurrentOutput (
	lockOutput,
	createProcessConcurrent,
) where

import System.IO
import System.Posix.IO
import Control.Monad
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Applicative
import System.IO.Unsafe (unsafePerformIO)
import Control.Concurrent
import Control.Concurrent.Async
import Data.Maybe
import Data.Char
import Data.List
import Data.Monoid
import qualified Data.ByteString as B
import qualified System.Process as P

import Utility.Monad
import Utility.Exception

data OutputHandle = OutputHandle
	{ outputLock :: MVar () -- ^ empty when locked
	, outputLockedBy :: MVar Locker
	}

data Locker
	= GeneralLock
	| ProcessLock P.ProcessHandle

-- | A shared global variable for the OutputHandle.
{-# NOINLINE globalOutputHandle #-}
globalOutputHandle :: MVar OutputHandle
globalOutputHandle = unsafePerformIO $ 
	newMVar =<< OutputHandle
		<$> newMVar ()
		<*> newEmptyMVar

-- | Gets the global OutputHandle.
getOutputHandle :: IO OutputHandle
getOutputHandle = readMVar globalOutputHandle

-- | Holds a lock while performing an action. Any other threads
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
	lck <- outputLock <$> getOutputHandle
	go =<< tryTakeMVar lck
  where
	-- lck was full, and we've emptied it, so we hold the lock now.
	go (Just ()) = havelock
	-- lck is empty, so someone else is holding the lock.
	go Nothing = do
		lcker <- outputLockedBy <$> getOutputHandle
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
		lck <- outputLock <$> getOutputHandle
		takeMVar lck
		havelock
	whenblock a = if block then a else return False

-- | Only safe to call after taking the output lock.
dropOutputLock :: IO ()
dropOutputLock = do
	lcker <- outputLockedBy <$> getOutputHandle
	lck <- outputLock <$> getOutputHandle
	void $ takeMVar lcker
	putMVar lck ()

-- | Only safe to call after takeOutputLock; updates the Locker.
updateOutputLocker :: Locker -> IO ()
updateOutputLocker l = do
	lcker <- outputLockedBy <$> getOutputHandle
	void $ tryTakeMVar lcker
	putMVar lcker l
	modifyMVar_ lcker (const $ return l)

-- | Wrapper around `System.Process.createProcess` that prevents 
-- multiple processes that are running concurrently from writing
-- to stdout/stderr at the same time.
--
-- The first process is allowed to write to stdout and stderr in the usual way.
--
-- However, if another process runs concurrently with the
-- first, any stdout or stderr that would have been displayed by it is
-- instead buffered. The buffered output will be displayed the next time it
-- is safe to do so (ie, after the first process exits).
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

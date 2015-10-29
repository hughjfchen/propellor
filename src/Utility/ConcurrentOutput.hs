{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

-- | Concurrent output handling.

module Utility.ConcurrentOutput (
	withConcurrentOutput,
	flushConcurrentOutput,
	outputConcurrent,
	createProcessConcurrent,
	waitForProcessConcurrent,
	lockOutput,
) where

import System.IO
import System.Posix.IO
import System.Directory
import System.Exit
import Control.Monad
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Applicative
import System.IO.Unsafe (unsafePerformIO)
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.Async
import Data.Maybe
import Data.List
import Data.Monoid
import qualified Data.ByteString as B
import qualified System.Process as P
import qualified Data.Set as S

import Utility.Monad
import Utility.Exception
import Utility.FileSystemEncoding

data OutputHandle = OutputHandle
	{ outputLock :: TMVar Lock
	, outputBuffer :: TMVar Buffer
	, outputThreads :: TMVar (S.Set (Async ()))
	}

data Lock = Locked

-- | A shared global variable for the OutputHandle.
{-# NOINLINE globalOutputHandle #-}
globalOutputHandle :: MVar OutputHandle
globalOutputHandle = unsafePerformIO $ 
	newMVar =<< OutputHandle
		<$> newEmptyTMVarIO
		<*> newTMVarIO []
		<*> newTMVarIO S.empty

-- | Gets the global OutputHandle.
getOutputHandle :: IO OutputHandle
getOutputHandle = readMVar globalOutputHandle

-- | Holds a lock while performing an action that will display output.
-- While this is running, other threads that try to lockOutput will block,
-- and calls to `outputConcurrent` and `createProcessConcurrent`
-- will result in that concurrent output being buffered and not
-- displayed until the action is done.
lockOutput :: (MonadIO m, MonadMask m) => m a -> m a
lockOutput = bracket_ (liftIO takeOutputLock) (liftIO dropOutputLock)

-- | Blocks until we have the output lock.
takeOutputLock :: IO ()
takeOutputLock = void $ takeOutputLock' True

-- | Tries to take the output lock, without blocking.
tryTakeOutputLock :: IO Bool
tryTakeOutputLock = takeOutputLock' False

withLock :: (TMVar Lock -> STM a) -> IO a
withLock a = do
	lck <- outputLock <$> getOutputHandle
	atomically (a lck)

takeOutputLock' :: Bool -> IO Bool
takeOutputLock' block = do
	locked <- withLock $ \l -> do
		v <- tryTakeTMVar l
		case v of
			Just Locked
				| block -> retry
				| otherwise -> do
					-- Restore value we took.
					putTMVar l Locked
					return False
			Nothing -> do
				putTMVar l Locked
				return True
	when locked $ do
		bv <- outputBuffer <$> getOutputHandle
		buf <- atomically $ swapTMVar bv []
		emitBuffer stdout buf
	return locked

-- | Only safe to call after taking the output lock.
dropOutputLock :: IO ()
dropOutputLock = withLock $ void . takeTMVar

-- | Use this around any IO actions that use `outputConcurrent`
-- or `createProcessConcurrent`
--
-- This is necessary to ensure that buffered concurrent output actually
-- gets displayed before the program exits.
withConcurrentOutput :: IO a -> IO a
withConcurrentOutput a = a `finally` flushConcurrentOutput

-- | Blocks until any processes started by `createProcessConcurrent` have
-- finished, and any buffered output is displayed.
flushConcurrentOutput :: IO ()
flushConcurrentOutput = do
	-- Wait for all outputThreads to finish.
	v <- outputThreads <$> getOutputHandle
	atomically $ do
		r <- takeTMVar v
		if r == S.empty
			then putTMVar v r
			else retry
	-- Take output lock to ensure that nothing else is currently
	-- generating output, and flush any buffered output.
	lockOutput $ return ()

-- | Displays a string to stdout, and flush output so it's displayed.
--
-- Uses locking to ensure that the whole string is output atomically
-- even when other threads are concurrently generating output.
--
-- When something else is writing to the console at the same time, this does
-- not block. It buffers the string, so it will be displayed once the other
-- writer is done.
outputConcurrent :: String -> IO ()
outputConcurrent s = bracket setup cleanup go
  where
	setup = tryTakeOutputLock
	cleanup False = return ()
	cleanup True = dropOutputLock
	go True = do
		putStr s
		hFlush stdout
	go False = do
		bv <- outputBuffer <$> getOutputHandle
		oldbuf <- atomically $ takeTMVar bv
		newbuf <- addBuffer (Output (B.pack (decodeW8NUL s))) oldbuf
		atomically $ putTMVar bv newbuf

-- | This must be used to wait for processes started with 
-- `createProcessConcurrent`.
--
-- This is necessary because `System.Process.waitForProcess` has a
-- race condition when two threads check the same process. If the race
-- is triggered, one thread will successfully wait, but the other
-- throws a DoesNotExist exception.
waitForProcessConcurrent :: P.ProcessHandle -> IO ExitCode
waitForProcessConcurrent h = do
	v <- tryWhenExists (P.waitForProcess h)
	case v of
		Just r -> return r
		Nothing -> maybe (waitForProcessConcurrent h) return =<< P.getProcessExitCode h

-- | Wrapper around `System.Process.createProcess` that prevents 
-- multiple processes that are running concurrently from writing
-- to stdout/stderr at the same time.
--
-- If the process does not output to stdout or stderr, it's run
-- by createProcess entirely as usual. Only processes that can generate
-- output are handled specially:
--
-- A process is allowed to write to stdout and stderr in the usual
-- way, assuming it can successfully take the output lock.
--
-- When the output lock is held (by another concurrent process,
-- or because `outputConcurrent` is being called at the same time),
-- the process is instead run with its stdout and stderr
-- redirected to a buffer. The buffered output will be displayed as soon
-- as the output lock becomes free.
createProcessConcurrent :: P.CreateProcess -> IO (Maybe Handle, Maybe Handle, Maybe Handle, P.ProcessHandle) 
createProcessConcurrent p
	| willOutput (P.std_out p) || willOutput (P.std_err p) =
		ifM tryTakeOutputLock
			( firstprocess
			, concurrentprocess
			)
	| otherwise = P.createProcess p
  where
	rediroutput ss h
		| willOutput ss = P.UseHandle h
		| otherwise = ss

	firstprocess = do
		r@(_, _, _, h) <- P.createProcess p
			`onException` dropOutputLock
		-- Wait for the process to exit and drop the lock.
		void $ async $ do
			void $ tryIO $ waitForProcessConcurrent h
			dropOutputLock
		return r
	
	concurrentprocess = do
		(toouth, fromouth) <- pipe
		(toerrh, fromerrh) <- pipe
		let p' = p
			{ P.std_out = rediroutput (P.std_out p) toouth
			, P.std_err = rediroutput (P.std_err p) toerrh
			}
		r <- P.createProcess p'
		outbuf <- setupBuffer stdout toouth (P.std_out p) fromouth
		errbuf <- setupBuffer stderr toerrh (P.std_err p) fromerrh
		void $ async $ bufferWriter [outbuf, errbuf]
		return r

	pipe = do
		(from, to) <- createPipe
		(,) <$> fdToHandle to <*> fdToHandle from

willOutput :: P.StdStream -> Bool
willOutput P.Inherit = True
willOutput _ = False

-- Built up with newest seen output first.
type Buffer = [BufferedActivity]

data BufferedActivity
	= ReachedEnd
	| Output B.ByteString
	| InTempFile FilePath
	deriving (Eq)

setupBuffer :: Handle -> Handle -> P.StdStream -> Handle -> IO (Handle, MVar Buffer, TMVar ())
setupBuffer h toh ss fromh = do
	hClose toh
	buf <- newMVar []
	bufsig <- atomically newEmptyTMVar
	void $ async $ outputDrainer ss fromh buf bufsig
	return (h, buf, bufsig)

-- Drain output from the handle, and buffer it.
outputDrainer :: P.StdStream -> Handle -> MVar Buffer -> TMVar () -> IO ()
outputDrainer ss fromh buf bufsig
	| willOutput ss = go
	| otherwise = atend
  where
	go = do
		v <- tryIO $ B.hGetSome fromh 1048576
		case v of
			Right b | not (B.null b) -> do
				modifyMVar_ buf $ addBuffer (Output b)
				changed
				go
			_ -> atend
	atend = do
		modifyMVar_ buf $ pure . (ReachedEnd :)
		changed
		hClose fromh
	changed = atomically $ do
		void $ tryTakeTMVar bufsig
		putTMVar bufsig ()

-- Wait to lock output, and once we can, display everything 
-- that's put into the buffers, until the end.
bufferWriter :: [(Handle, MVar Buffer, TMVar ())] -> IO ()
bufferWriter ts = do
	worker <- async $ void $ lockOutput $ mapConcurrently go ts
	v <- outputThreads <$> getOutputHandle
	atomically $ do
		s <- takeTMVar v
		putTMVar v (S.insert worker s)
	void $ async $ do
		void $ waitCatch worker
		atomically $ do
			s <- takeTMVar v
			putTMVar v (S.delete worker s)
  where
	go v@(outh, buf, bufsig) = do
		void $ atomically $ takeTMVar bufsig
		l <- takeMVar buf
		putMVar buf []
		emitBuffer outh l
		if any (== ReachedEnd) l
			then return ()
			else go v

emitBuffer :: Handle -> Buffer -> IO ()
emitBuffer outh l = forM_ (reverse l) $ \ba -> case ba of
	Output b -> do
		B.hPut outh b
		hFlush outh
	InTempFile tmp -> do
		B.hPut outh =<< B.readFile tmp
		void $ tryWhenExists $ removeFile tmp
	ReachedEnd -> return ()

-- Adds a value to the Buffer. When adding Output to a Handle, it's cheaper
-- to combine it with any already buffered Output to that same Handle.
--
-- When the total buffered Output exceeds 1 mb in size, it's moved out of
-- memory, to a temp file. This should only happen rarely, but is done to
-- avoid some verbose process unexpectedly causing excessive memory use.
addBuffer :: BufferedActivity -> Buffer -> IO Buffer
addBuffer (Output b) buf
	| B.length b' <= 1048576 = return (Output b' : other)
	| otherwise = do
		tmpdir <- getTemporaryDirectory
		(tmp, h) <- openTempFile tmpdir "output.tmp"
		B.hPut h b'
		hClose h
		return (InTempFile tmp : other)
  where
	!b' = B.concat (mapMaybe getOutput this) <> b
	!(this, other) = partition isOutput buf
	isOutput v = case v of
		Output _ -> True
		_ -> False
	getOutput v = case v of
		Output b'' -> Just b''
		_ -> Nothing
addBuffer v buf = return (v:buf)

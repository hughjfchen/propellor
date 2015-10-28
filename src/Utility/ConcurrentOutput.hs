-- | Concurrent output handling.

module Utility.ConcurrentOutput (
	withConcurrentOutput,
	outputConcurrent,
	createProcessConcurrent,
) where

import System.IO
import System.Posix.IO
import System.Directory
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

import Utility.Monad
import Utility.Exception

data OutputHandle = OutputHandle
	{ outputLock :: TMVar (Maybe Locker)
	}

data Locker
	= GeneralLock
	| ProcessLock P.ProcessHandle String

instance Show Locker where
	show GeneralLock = "GeneralLock"
	show (ProcessLock _ cmd) = "ProcessLock " ++ cmd

-- | A shared global variable for the OutputHandle.
{-# NOINLINE globalOutputHandle #-}
globalOutputHandle :: MVar OutputHandle
globalOutputHandle = unsafePerformIO $ 
	newMVar =<< OutputHandle
		<$> newTMVarIO Nothing

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

withLock :: (TMVar (Maybe Locker) -> STM a) -> IO a
withLock a = do
	lck <- outputLock <$> getOutputHandle
	atomically (a lck)

-- The lock TMVar is kept full normally, even if only with Nothing,
-- so if we take it here, that blocks anyone else from trying
-- to take the lock while we are checking it.
takeOutputLock' :: Bool -> IO Bool
takeOutputLock' block = go =<< withLock tryTakeTMVar
  where
	go Nothing = whenblock waitlock
	-- Something has the lock. It may be stale, so check it.
	-- We must always be sure to fill the TMVar back with Just or Nothing.
	go (Just orig) = case orig of
		Nothing -> havelock
		(Just (ProcessLock h _)) ->
			-- when process has exited, lock is stale
			ifM (isJust <$> P.getProcessExitCode h)
				( havelock
				, if block
					then do
						hPutStr stderr "WAITFORPROCESS in lock"
						hFlush stderr
						void $ P.waitForProcess h
						hPutStr stderr "WAITFORPROCESS in lock done"
						hFlush stderr
						havelock
					else do
						withLock (`putTMVar` orig)
						return False
				)
		(Just GeneralLock) -> do
			withLock (`putTMVar` orig)
			whenblock waitlock

	havelock = do
		withLock (`putTMVar` Just GeneralLock)
		return True
	
	-- Wait for current lock holder (if any) to relinquish
	-- it and take the lock for ourselves.
	waitlock = withLock $ \l -> do
		v <- tryTakeTMVar l
		case v of
			Just (Just _) -> retry
			_ -> do
				putTMVar l (Just GeneralLock)
				return True
	
	whenblock a = if block then a else return False

-- | Only safe to call after taking the output lock.
dropOutputLock :: IO ()
dropOutputLock = withLock $ \l -> do
	void $ takeTMVar l
	putTMVar l Nothing

-- | Only safe to call after takeOutputLock; updates the Locker.
updateOutputLocker :: Locker -> IO ()
updateOutputLocker locker = withLock $ \l -> do
	void $ takeTMVar l
	putTMVar l (Just locker)

-- | Use this around any IO actions that use `outputConcurrent`
-- or `createProcessConcurrent`
--
-- This is necessary to ensure that buffered concurrent output actually
-- gets displayed before the program exits.
withConcurrentOutput :: IO a -> IO a
withConcurrentOutput a = a `finally` drain
  where
	-- Just taking the output lock is enough to ensure that anything
	-- that was buffering output has had a chance to flush its buffer.
	drain = do
		hPutStrLn stderr "DRAIN"
		hFlush stderr
		lockOutput (return ())
		hPutStrLn stderr "DRAIN DONE"
		hFlush stderr

-- | Displays a string to stdout, and flush output so it's displayed.
--
-- Uses locking to ensure that the whole string is output atomically
-- even when other threads are concurrently generating output.
--
-- When something else is writing to the console at the same time, this does
-- not block. It buffers the string, so it will be displayed once the other
-- writer is done.
outputConcurrent :: String -> IO ()
outputConcurrent s = do
	putStr s
	hFlush stdout
	-- TODO

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
	| willoutput (P.std_out p) || willoutput (P.std_err p) =
		ifM tryTakeOutputLock
			( do
				hPutStrLn stderr $ show ("NOT CONCURRENT", cmd)
				hFlush stderr
				firstprocess
			, do
				v <- withLock $ tryReadTMVar
				hPutStrLn stderr $ show ("IS CONCURRENT", cmd, v)
				hFlush stderr
				concurrentprocess
			)
	| otherwise = P.createProcess p
  where
	willoutput P.Inherit = True
	willoutput _ = False

	rediroutput str h
		| willoutput str = P.UseHandle h
		| otherwise = str

	cmd = case P.cmdspec p of
		P.ShellCommand s -> s
		P.RawCommand c ps -> unwords (c:ps)

	firstprocess = do
		r@(_, _, _, h) <- P.createProcess p
			`onException` dropOutputLock
		updateOutputLocker (ProcessLock h cmd)
		-- Output lock is still held as we return; the process
		-- is running now, and once it exits the output lock will
		-- be stale and can then be taken by something else.
		return r
	
	concurrentprocess = do
		(toouth, fromouth) <- pipe
		(toerrh, fromerrh) <- pipe
		let p' = p
			{ P.std_out = rediroutput (P.std_out p) toouth
			, P.std_err = rediroutput (P.std_err p) toerrh
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

type Buffer = [(Handle, BufferedActivity)]

data BufferedActivity
	= ReachedEnd
	| Output B.ByteString
	| InTempFile FilePath
	deriving (Eq)

-- Drain output from the handle, and buffer it in memory.
outputDrainer :: Handle -> Handle -> MVar Buffer -> IO ()
outputDrainer fromh toh buf = do
	v <- tryIO $ B.hGetSome fromh 1024
	case v of
		Right b | not (B.null b) -> do
			modifyMVar_ buf $ addBuffer (toh, Output b)
			outputDrainer fromh toh buf
		_ -> do
			modifyMVar_ buf $ pure . (++ [(toh, ReachedEnd)])
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
		forM_ l $ \(h, ba) -> case ba of
			Output b -> do
				B.hPut h b
				hFlush h
			InTempFile tmp -> do
				B.hPut h =<< B.readFile tmp
				void $ tryWhenExists $ removeFile tmp
			ReachedEnd -> return ()
		let hs' = filter (\h -> not (any (== (h, ReachedEnd)) l)) hs
		putMVar buf []
		go hs'

-- Adds a value to the Buffer. When adding Output to a Handle, it's cheaper
-- to combine it with any already buffered Output to that same Handle.
--
-- When the total buffered Output exceeds 1 mb in size, it's moved out of
-- memory, to a temp file. This should only happen rarely, but is done to
-- avoid some verbose process unexpectedly causing excessive memory use.
addBuffer :: (Handle, BufferedActivity) -> Buffer -> IO Buffer
addBuffer (toh, Output b) buf
	| B.length b' <= 1000000 = return ((toh, Output b') : other)
	| otherwise = do
		tmpdir <- getTemporaryDirectory
		(tmp, h) <- openTempFile tmpdir "output.tmp"
		B.hPut h b'
		hClose h
		return ((toh, InTempFile tmp) : other)
  where
	b' = B.concat (mapMaybe getOutput this) <> b
	(this, other) = partition same buf
	same v = fst v == toh && case snd v of
		Output _ -> True
		_ -> False
	getOutput v = case snd v of
		Output b'' -> Just b''
		_ -> Nothing
addBuffer v buf = return (buf ++ [v])

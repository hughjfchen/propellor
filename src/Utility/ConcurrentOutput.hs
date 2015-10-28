-- | Concurrent output handling.

module Utility.ConcurrentOutput (
	lockOutput,
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
import Control.Concurrent.Async
import Data.Maybe
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
-- If the process does not output to stdout or stderr, it's run
-- by createProcess entirely as usual. Only processes that can generate
-- output are handled specially:
--
-- A process is allowed to write to stdout and stderr in the usual
-- way, assuming it can successfully take the output lock.
--
-- When the output lock is held (by another process or other caller of
-- `lockOutput`), the process is instead run with its stdout and stderr
-- redirected to a buffer. The buffered output will be displayed as soon
-- as the output lock becomes free.
createProcessConcurrent :: P.CreateProcess -> IO (Maybe Handle, Maybe Handle, Maybe Handle, P.ProcessHandle) 
createProcessConcurrent p
	| willoutput (P.std_out p) || willoutput (P.std_err p) =
		ifM tryTakeOutputLock
			( firstprocess
			, concurrentprocess
			)
	| otherwise = P.createProcess p
  where
	willoutput P.Inherit = True
	willoutput _ = False

	rediroutput str h
		| willoutput str = P.UseHandle h
		| otherwise = str

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

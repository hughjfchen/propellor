-- | 
-- Copyright: 2015 Joey Hess <id@joeyh.name>
-- License: BSD-2-clause
-- 
-- Concurrent output handling.
--
-- > import Control.Concurrent.Async
-- > import System.Console.Concurrent
-- >
-- > main = withConcurrentOutput $
-- > 	outputConcurrent "washed the car\n"
-- > 		`concurrently`
-- >	outputConcurrent "walked the dog\n"
-- >		`concurrently`
-- > 	createProcessConcurrent (proc "ls" [])

module System.Console.Concurrent (
	-- * Concurrent output
	withConcurrentOutput,
	Outputable(..),
	outputConcurrent,
	ConcurrentProcessHandle,
	createProcessConcurrent,
	waitForProcessConcurrent,
	createProcessForeground,
	flushConcurrentOutput,
	lockOutput,
	-- * Low level access to the output buffer
	OutputBuffer,
	StdHandle(..),
	bufferOutputSTM,
	outputBufferWaiterSTM,
	waitAnyBuffer,
	waitCompleteLines,
	emitOutputBuffer,
) where

import System.Console.Concurrent.Internal


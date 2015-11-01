module Utility.Process.Shim (module X, createProcess, waitForProcess) where

import System.Process as X hiding (createProcess, waitForProcess)
import Utility.ConcurrentOutput (createProcessConcurrent, waitForProcessConcurrent)
import System.IO
import System.Exit

createProcess :: CreateProcess -> IO (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle) 
createProcess = createProcessConcurrent

waitForProcess :: ProcessHandle -> IO ExitCode
waitForProcess = waitForProcessConcurrent

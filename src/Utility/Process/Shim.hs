module Utility.Process.Shim (module X, createProcess, waitForProcess) where

import System.Process as X hiding (createProcess, waitForProcess)
import System.Process.Concurrent

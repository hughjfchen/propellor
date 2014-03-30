module Property.Hostname where

import Data.List
import System.Posix
import Control.Applicative
import Data.Maybe

import Property
import Utility.SafeCommand
import Utility.Exception

type HostName = String

set :: HostName -> Property
set hostname = fileHasContent "/etc/hostname" [hostname]

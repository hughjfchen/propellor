{-# LANGUAGE PackageImports #-}

-- | Pulls in lots of useful modules for building and using Properties.
-- 
-- When propellor runs on a Host, it ensures that its list of Properties
-- is satisfied, taking action as necessary when a Property is not
-- currently satisfied.
--
-- A simple propellor program example:
--
-- > import Propellor
-- > import Propellor.CmdLine
-- > import qualified Propellor.Property.File as File
-- > import qualified Propellor.Property.Apt as Apt
-- > 
-- > main :: IO ()
-- > main = defaultMain hosts
-- > 
-- > hosts :: [Host]
-- > hosts =
-- >   [ host "example.com"
-- >     & Apt.installed ["mydaemon"]
-- >     & "/etc/mydaemon.conf" `File.containsLine` "secure=1"
-- >       `onChange` cmdProperty "service" ["mydaemon", "restart"]
-- >     ! Apt.installed ["unwantedpackage"]
-- >   ]
--
-- See config.hs for a more complete example, and clone Propellor's
-- git repository for a deployable system using Propellor:
-- git clone <git://git.kitenet.net/propellor>

module Propellor (
	  module Propellor.Types
	, module Propellor.Property
	, module Propellor.Property.Cmd
	, module Propellor.Info
	, module Propellor.PrivData
	, module Propellor.Engine
	, module Propellor.Exception
	, module Propellor.Message
	, localdir

	, module X
) where

import Propellor.Types
import Propellor.Property
import Propellor.Engine
import Propellor.Property.Cmd
import Propellor.PrivData
import Propellor.Message
import Propellor.Exception
import Propellor.Info

import Utility.PartialPrelude as X
import Utility.Process as X
import Utility.Exception as X
import Utility.Env as X
import Utility.Directory as X
import Utility.Tmp as X
import Utility.Monad as X
import Utility.Misc as X

import System.Directory as X
import System.IO as X
import System.FilePath as X
import Data.Maybe as X
import Data.Either as X
import Control.Applicative as X
import Control.Monad as X
import Data.Monoid as X
import Control.Monad.IfElse as X
import "mtl" Control.Monad.Reader as X

-- | This is where propellor installs itself when deploying a host.
localdir :: FilePath
localdir = "/usr/local/propellor"

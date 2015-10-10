{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

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
-- git clone <git://git.joeyh.name/propellor>

module Propellor (
	-- * Core data types
	  Host(..)
	, Property
	, RevertableProperty
	, (<!>)
	-- * Defining a Host and its properties
	, host
	, (&)
	, (!)
	-- * Combining properties
	-- | Properties are often combined together in your propellor
	-- configuration. For example:
	--
	-- > "/etc/foo/config" `File.containsLine` "bar=1"
	-- > 	`requires` File.dirExists "/etc/foo"
	, requires
	, before
	, onChange
	-- * Included modules
	, module Propellor.Types
	, module Propellor.Property
	-- | Everything you need to build your own properties,
	-- and useful property combinators
	, module Propellor.Property.Cmd
	-- | Properties to run shell commands
	, module Propellor.Property.List
	-- | Combining a list of properties into a single property
	, module Propellor.Types.PrivData
	-- | Private data access for properties
	, module Propellor.PropAccum
	, module Propellor.Info
	, module Propellor.PrivData
	, module Propellor.Engine
	, module Propellor.Exception
	, module Propellor.Message
	, module Propellor.Location

	, module X
) where

import Propellor.Types
import Propellor.Property
import Propellor.Engine
import Propellor.Property.List
import Propellor.Property.Cmd
import Propellor.PrivData
import Propellor.Types.PrivData
import Propellor.Message
import Propellor.Exception
import Propellor.Info
import Propellor.PropAccum
import Propellor.Location

-- Things imported as X won't be included in the haddock for this page,
-- but will be re-exported silently.
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

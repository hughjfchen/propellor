module Propellor.Types where

import Data.Monoid
import System.Console.ANSI

type HostName = String
type UserName = String

data Property = Property
	{ propertyDesc :: Desc
	-- | must be idempotent; may run repeatedly
	, propertySatisfy :: IO Result
	}

type Desc = String

data Result = NoChange | MadeChange | FailedChange
	deriving (Read, Show, Eq)

instance Monoid Result where
	mempty = NoChange

	mappend FailedChange _ = FailedChange
	mappend _ FailedChange = FailedChange
	mappend MadeChange _ = MadeChange
	mappend _ MadeChange = MadeChange
	mappend NoChange NoChange = NoChange

class ActionResult a where
	getActionResult :: a -> (String, ColorIntensity, Color)

instance ActionResult Bool where
	getActionResult False = ("failed", Vivid, Red)
	getActionResult True = ("ok", Dull, Green)

instance ActionResult Result where
	getActionResult NoChange = ("unchanged", Dull, Green)
	getActionResult MadeChange = ("done", Vivid, Green)
	getActionResult FailedChange = ("failed", Vivid, Red)

data CmdLine
	= Run HostName
	| Spin HostName
	| Boot HostName
	| Set HostName PrivDataField
	| AddKey String
	| Continue CmdLine
	| Chain HostName
	| ChainDocker HostName
  deriving (Read, Show, Eq)

-- | Note that removing or changing field names will break the
-- serialized privdata files, so don't do that!
-- It's fine to add new fields.
data PrivDataField
	= DockerAuthentication
	| SshPrivKey UserName
	| Password UserName
	deriving (Read, Show, Ord, Eq)

{-# LANGUAGE PackageImports #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Propellor.Types
	( Host(..)
	, Info
	, getInfo
	, Propellor(..)
	, Property(..)
	, RevertableProperty(..)
	, IsProp
	, describe
	, toProp
	, requires
	, Desc
	, Result(..)
	, ActionResult(..)
	, CmdLine(..)
	, PrivDataField(..)
	, GpgKeyId
	, SshKeyType(..)
	, module Propellor.Types.OS
	, module Propellor.Types.Dns
	) where

import Data.Monoid
import Control.Applicative
import System.Console.ANSI
import "mtl" Control.Monad.Reader
import "MonadCatchIO-transformers" Control.Monad.CatchIO

import Propellor.Types.Info
import Propellor.Types.OS
import Propellor.Types.Dns

-- | Everything Propellor knows about a system: Its hostname,
-- properties and other info.
data Host = Host
	{ hostName :: HostName
	, hostProperties :: [Property]
	, hostInfo :: Info
	}
	deriving (Show)

-- | Propellor's monad provides read-only access to info about the host
-- it's running on.
newtype Propellor p = Propellor { runWithHost :: ReaderT Host IO p }
	deriving
		( Monad
		, Functor
		, Applicative
		, MonadReader Host
		, MonadIO
		, MonadCatchIO
		)

-- | The core data type of Propellor, this represents a property
-- that the system should have, and an action to ensure it has the
-- property.
data Property = Property
	{ propertyDesc :: Desc
	, propertySatisfy :: Propellor Result
	-- ^ must be idempotent; may run repeatedly
	, propertyInfo :: Info
	-- ^ a property can add info to the host.
	}

instance Show Property where
	show p = "property " ++ show (propertyDesc p)

-- | A property that can be reverted.
data RevertableProperty = RevertableProperty Property Property

class IsProp p where
	-- | Sets description.
	describe :: p -> Desc -> p
	toProp :: p -> Property
	-- | Indicates that the first property can only be satisfied
	-- once the second one is.
	requires :: p -> Property -> p
	getInfo :: p -> Info

instance IsProp Property where
	describe p d = p { propertyDesc = d }
	toProp p = p
	getInfo = propertyInfo
	x `requires` y = Property (propertyDesc x) satisfy info
	  where
	  	info = getInfo y <> getInfo x
		satisfy = do
			r <- propertySatisfy y
			case r of
				FailedChange -> return FailedChange
				_ -> propertySatisfy x
			

instance IsProp RevertableProperty where
	-- | Sets the description of both sides.
	describe (RevertableProperty p1 p2) d = 
		RevertableProperty (describe p1 d) (describe p2 ("not " ++ d))
	toProp (RevertableProperty p1 _) = p1
	(RevertableProperty p1 p2) `requires` y =
		RevertableProperty (p1 `requires` y) p2
	-- | Return the Info of the currently active side.
	getInfo (RevertableProperty p1 _p2) = getInfo p1

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

-- | Results of actions, with color.
class ActionResult a where
	getActionResult :: a -> (String, ColorIntensity, Color)

instance ActionResult Bool where
	getActionResult False = ("failed", Vivid, Red)
	getActionResult True = ("done", Dull, Green)

instance ActionResult Result where
	getActionResult NoChange = ("ok", Dull, Green)
	getActionResult MadeChange = ("done", Vivid, Green)
	getActionResult FailedChange = ("failed", Vivid, Red)

data CmdLine
	= Run HostName
	| Spin HostName
	| Boot HostName
	| Set HostName PrivDataField
	| Dump HostName PrivDataField
	| AddKey String
	| Continue CmdLine
	| Chain HostName
	| Docker HostName
  deriving (Read, Show, Eq)

-- | Note that removing or changing field names will break the
-- serialized privdata files, so don't do that!
-- It's fine to add new fields.
data PrivDataField
	= DockerAuthentication
	| SshPubKey SshKeyType UserName
	| SshPrivKey SshKeyType UserName
	| SshAuthorizedKeys UserName
	| Password UserName
	| PrivFile FilePath
	| GpgKey GpgKeyId
	deriving (Read, Show, Ord, Eq)

type GpgKeyId = String

data SshKeyType = SshRsa | SshDsa | SshEcdsa | SshEd25519
	deriving (Read, Show, Ord, Eq)

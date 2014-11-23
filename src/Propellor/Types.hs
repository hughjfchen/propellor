{-# LANGUAGE PackageImports #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Propellor.Types
	( Host(..)
	, Info(..)
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
	, PrivData
	, Context(..)
	, anyContext
	, SshKeyType(..)
	, Val(..)
	, fromVal
	, module Propellor.Types.OS
	, module Propellor.Types.Dns
	) where

import Data.Monoid
import Control.Applicative
import System.Console.ANSI
import System.Posix.Types
import "mtl" Control.Monad.Reader
import "MonadCatchIO-transformers" Control.Monad.CatchIO
import qualified Data.Set as S
import qualified Propellor.Types.Dns as Dns

import Propellor.Types.OS
import Propellor.Types.Chroot
import Propellor.Types.Dns
import Propellor.Types.Docker
import Propellor.Types.PrivData

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
	| Spin [HostName] (Maybe HostName)
	| SimpleRun HostName
	| Set PrivDataField Context
	| Dump PrivDataField Context
	| Edit PrivDataField Context
	| ListFields
	| AddKey String
	| Merge
	| Serialized CmdLine
	| Continue CmdLine
	| Update (Maybe HostName)
	| DockerInit HostName
	| DockerChain HostName String
	| ChrootChain HostName FilePath Bool Bool
	| GitPush Fd Fd
	deriving (Read, Show, Eq)

-- | Information about a host.
data Info = Info
	{ _os :: Val System
	, _privDataFields :: S.Set (PrivDataField, Context)
	, _sshPubKey :: Val String
	, _aliases :: S.Set HostName
	, _dns :: S.Set Dns.Record
	, _namedconf :: Dns.NamedConfMap
	, _dockerinfo :: DockerInfo Host
	, _chrootinfo :: ChrootInfo Host
	}
	deriving (Show)

instance Monoid Info where
	mempty = Info mempty mempty mempty mempty mempty mempty mempty mempty
	mappend old new = Info
		{ _os = _os old <> _os new
		, _privDataFields = _privDataFields old <> _privDataFields new
		, _sshPubKey = _sshPubKey old <> _sshPubKey new
		, _aliases = _aliases old <> _aliases new
		, _dns = _dns old <> _dns new
		, _namedconf = _namedconf old <> _namedconf new
		, _dockerinfo = _dockerinfo old <> _dockerinfo new
		, _chrootinfo = _chrootinfo old <> _chrootinfo new
		}

data Val a = Val a | NoVal
	deriving (Eq, Show)

instance Monoid (Val a) where
	mempty = NoVal
	mappend old new = case new of
		NoVal -> old
		_ -> new

fromVal :: Val a -> Maybe a
fromVal (Val a) = Just a
fromVal NoVal = Nothing

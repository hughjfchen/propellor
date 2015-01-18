{-# LANGUAGE PackageImports #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Propellor.Types
	( Host(..)
	, Info(..)
	, getInfoRecursive
	, Propellor(..)
	, Property(..)
	, RevertableProperty(..)
	, IsProp
	, describe
	, toProp
	, requires
	, Desc
	, Result(..)
	, ToResult(..)
	, ActionResult(..)
	, CmdLine(..)
	, PrivDataField(..)
	, PrivData
	, Context(..)
	, anyContext
	, SshKeyType(..)
	, Val(..)
	, fromVal
	, RunLog
	, EndAction(..)
	, module Propellor.Types.OS
	, module Propellor.Types.Dns
	) where

import Data.Monoid
import Control.Applicative
import System.Console.ANSI
import System.Posix.Types
import "mtl" Control.Monad.RWS.Strict
import "MonadCatchIO-transformers" Control.Monad.CatchIO
import qualified Data.Set as S
import qualified Data.Map as M

import Propellor.Types.OS
import Propellor.Types.Chroot
import Propellor.Types.Dns
import Propellor.Types.Docker
import Propellor.Types.PrivData
import Propellor.Types.Empty
import qualified Propellor.Types.Dns as Dns

-- | Everything Propellor knows about a system: Its hostname,
-- properties and their collected info.
data Host = Host
	{ hostName :: HostName
	, hostProperties :: [Property]
	, hostInfo :: Info
	}
	deriving (Show)

-- | Propellor's monad provides read-only access to info about the host
-- it's running on, and a writer to accumulate logs about the run.
newtype Propellor p = Propellor { runWithHost :: RWST Host RunLog () IO p }
	deriving
		( Monad
		, Functor
		, Applicative
		, MonadReader Host
		, MonadWriter RunLog
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
	-- ^ info associated with the property
	, propertyChildren :: [Property]
	-- ^ A property can include a list of child properties.
	-- This allows them to be introspected to collect their info,
	-- etc.
	--
	-- Note that listing Properties here does not ensure that
	-- their propertySatisfy is run when satisfying the parent
	-- property; it's up to the parent's propertySatisfy to do that.
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
	-- | Gets the info of the property, combined with all info
	-- of all children properties.
	getInfoRecursive :: p -> Info

instance IsProp Property where
	describe p d = p { propertyDesc = d }
	toProp p = p
	getInfoRecursive p = propertyInfo p <> mconcat (map getInfoRecursive (propertyChildren p))
	x `requires` y = x 
		{ propertySatisfy = do
			r <- propertySatisfy y
			case r of
				FailedChange -> return FailedChange
				_ -> propertySatisfy x
		, propertyChildren = y : propertyChildren x
		}

instance IsProp RevertableProperty where
	-- | Sets the description of both sides.
	describe (RevertableProperty p1 p2) d = 
		RevertableProperty (describe p1 d) (describe p2 ("not " ++ d))
	toProp (RevertableProperty p1 _) = p1
	(RevertableProperty p1 p2) `requires` y =
		RevertableProperty (p1 `requires` y) p2
	-- | Return the Info of the currently active side.
	getInfoRecursive (RevertableProperty p1 _p2) = getInfoRecursive p1

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

class ToResult t where
	toResult :: t -> Result

instance ToResult Bool where
	toResult False = FailedChange
	toResult True = MadeChange

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
	| Relay HostName
	| DockerInit HostName
	| DockerChain HostName String
	| ChrootChain HostName FilePath Bool Bool
	| GitPush Fd Fd
	deriving (Read, Show, Eq)

-- | Information about a host.
data Info = Info
	{ _os :: Val System
	, _privData :: S.Set (PrivDataField, Maybe PrivDataSourceDesc, HostContext)
	, _sshPubKey :: M.Map SshKeyType String
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
		, _privData = _privData old <> _privData new
		, _sshPubKey = _sshPubKey new `M.union` _sshPubKey old
		, _aliases = _aliases old <> _aliases new
		, _dns = _dns old <> _dns new
		, _namedconf = _namedconf old <> _namedconf new
		, _dockerinfo = _dockerinfo old <> _dockerinfo new
		, _chrootinfo = _chrootinfo old <> _chrootinfo new
		}

instance Empty Info where
	isEmpty i = and
		[ isEmpty (_os i)
		, isEmpty (_privData i)
		, isEmpty (_sshPubKey i)
		, isEmpty (_aliases i)
		, isEmpty (_dns i)
		, isEmpty (_namedconf i)
		, isEmpty (_dockerinfo i)
		, isEmpty (_chrootinfo i)
		]

data Val a = Val a | NoVal
	deriving (Eq, Show)

instance Monoid (Val a) where
	mempty = NoVal
	mappend old new = case new of
		NoVal -> old
		_ -> new

instance Empty (Val a) where
	isEmpty NoVal = True
	isEmpty _ = False

fromVal :: Val a -> Maybe a
fromVal (Val a) = Just a
fromVal NoVal = Nothing

type RunLog = [EndAction]

-- | An action that Propellor runs at the end, after trying to satisfy all
-- properties. It's passed the combined Result of the entire Propellor run.
data EndAction = EndAction Desc (Result -> Propellor Result)

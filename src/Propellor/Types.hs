{-# LANGUAGE PackageImports #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE EmptyDataDecls #-}

module Propellor.Types
	( Host(..)
	, Property(..)
	, mkProperty
	, propertyDesc
	, propertySatisfy
	, propertyInfo
	, propertyChildren
	, RevertableProperty(..)
	, IsProp(..)
	, Desc
	, Info(..)
	, Propellor(..)
	, EndAction(..)
	, module Propellor.Types.OS
	, module Propellor.Types.Dns
	, module Propellor.Types.Result
	) where

import Data.Monoid
import Control.Applicative
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
import Propellor.Types.Val
import Propellor.Types.Result
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
-- it's running on, and a writer to accumulate EndActions.
newtype Propellor p = Propellor { runWithHost :: RWST Host [EndAction] () IO p }
	deriving
		( Monad
		, Functor
		, Applicative
		, MonadReader Host
		, MonadWriter [EndAction]
		, MonadIO
		, MonadCatchIO
		)

-- | An action that Propellor runs at the end, after trying to satisfy all
-- properties. It's passed the combined Result of the entire Propellor run.
data EndAction = EndAction Desc (Result -> Propellor Result)

-- | The core data type of Propellor, this represents a property
-- that the system should have, and an action to ensure it has the
-- property.
data Property = IProperty (GProperty HasInfo) | SProperty (GProperty NoInfo)

-- | Constructs a Property
mkProperty 
	:: Desc -- ^ description of the property
	-> Propellor Result -- ^ action to run to satisfy the property (must be idempotent; may run repeatedly)
	-> Info -- ^ info associated with the property
	-> [Property] -- ^ child properties
	-> Property
mkProperty d a i cs
	| isEmpty i && all isEmpty (map propertyInfo cs) = 
		SProperty (GSProperty d a cs)
	| otherwise = IProperty (GIProperty d a i cs)

instance Show Property where
        show p = "property " ++ show (propertyDesc p)

-- | This GADT allows creating operations that only act on Properties
-- that do not add Info to their Host.
data GProperty i where
	GIProperty :: Desc -> Propellor Result -> Info -> [Property] -> GProperty HasInfo
	GSProperty :: Desc -> Propellor Result -> [Property] -> GProperty NoInfo

data HasInfo
data NoInfo

propertyDesc :: Property -> Desc
propertyDesc (IProperty (GIProperty d _ _ _)) = d
propertyDesc (SProperty (GSProperty d _ _)) = d

propertySatisfy :: Property -> Propellor Result
propertySatisfy (IProperty (GIProperty _ a _ _)) = a
propertySatisfy (SProperty (GSProperty _ a _)) = a

propertyInfo :: Property -> Info
propertyInfo (IProperty (GIProperty _ _ i _)) = i
propertyInfo (SProperty _) = mempty

-- | A Property can include a list of child properties that it also
-- satisfies. This allows them to be introspected to collect their info, etc.
propertyChildren :: Property -> [Property]
propertyChildren (IProperty (GIProperty _ _ _ cs)) = cs
propertyChildren (SProperty (GSProperty _ _ cs)) = cs

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
	describe (IProperty (GIProperty _ a i cs)) d = 
		IProperty (GIProperty d a i cs)
	describe (SProperty (GSProperty _ a cs)) d =
		SProperty (GSProperty d a cs)
	toProp p = p
	getInfoRecursive (IProperty (GIProperty _ _ i cs)) = 
		i <> mconcat (map getInfoRecursive cs)
	getInfoRecursive (SProperty _) = mempty
	x `requires` y = mkProperty (propertyDesc x) satisfy (propertyInfo x) cs
	  where
		satisfy = do
			r <- propertySatisfy y
			case r of
				FailedChange -> return FailedChange
				_ -> propertySatisfy x
		cs = y : propertyChildren x

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

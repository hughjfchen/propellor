{-# LANGUAGE PackageImports #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Propellor.Types
	( Host(..)
	, Property(..)
	, HasInfo
	, NoInfo
	, Desc
	, mkProperty
	, propertyDesc
	, propertySatisfy
	, propertyInfo
	, propertyChildren
	, RevertableProperty(..)
	, mkRevertableProperty
	, requires
	, IsProp(..)
	, Info(..)
	, Propellor(..)
	, EndAction(..)
	, module Propellor.Types.OS
	, module Propellor.Types.Dns
	, module Propellor.Types.Result
	, ignoreInfo
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
	, hostProperties :: [Property HasInfo]
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

type Desc = String

-- | The core data type of Propellor, this represents a property
-- that the system should have, and an action to ensure it has the
-- property.
data Property i where
	IProperty :: Desc -> Propellor Result -> Info -> [Property HasInfo] -> Property HasInfo
	SProperty :: Desc -> Propellor Result -> [Property NoInfo] -> Property NoInfo

data HasInfo
data NoInfo

-- | Constructs a Property
mkProperty 
	:: Desc -- ^ description of the property
	-> Propellor Result -- ^ action to run to satisfy the property (must be idempotent; may run repeatedly)
	-> Info -- ^ info associated with the property
	-> [Property i] -- ^ child properties
	-> Property HasInfo
mkProperty d a i cs = IProperty d a i (map toIProperty cs)

toIProperty :: Property i -> Property HasInfo
toIProperty p@(IProperty {}) = p
toIProperty (SProperty d s cs) = IProperty d s mempty (map toIProperty cs)

toSProperty :: Property i -> Property NoInfo
toSProperty (IProperty d s _ cs) = SProperty d s (map toSProperty cs)
toSProperty p@(SProperty {}) = p

-- | Makes a version of a Proprty without its Info.
-- Use with caution!
ignoreInfo :: Property HasInfo -> Property NoInfo
ignoreInfo = toSProperty

instance Show (Property i) where
        show p = "property " ++ show (propertyDesc p)

propertyDesc :: Property i -> Desc
propertyDesc (IProperty d _ _ _) = d
propertyDesc (SProperty d _ _) = d

propertySatisfy :: Property i -> Propellor Result
propertySatisfy (IProperty _ a _ _) = a
propertySatisfy (SProperty _ a _) = a

propertyInfo :: Property i -> Info
propertyInfo (IProperty _ _ i _) = i
propertyInfo (SProperty {}) = mempty

-- | A Property can include a list of child properties that it also
-- satisfies. This allows them to be introspected to collect their info, etc.
propertyChildren :: Property i -> [Property i]
propertyChildren (IProperty _ _ _ cs) = cs
propertyChildren (SProperty _ _ cs) = cs

-- | A property that can be reverted.
data RevertableProperty = RevertableProperty (Property HasInfo) (Property HasInfo)

mkRevertableProperty :: Property i1 -> Property i2 -> RevertableProperty
mkRevertableProperty p1 p2 = RevertableProperty (toIProperty p1) (toIProperty p2)

class IsProp p where
	-- | Sets description.
	describe :: p -> Desc -> p
	toProp :: p -> Property HasInfo
	-- | Gets the info of the property, combined with all info
	-- of all children properties.
	getInfoRecursive :: p -> Info

instance IsProp (Property HasInfo) where
	describe (IProperty _ a i cs) d = IProperty d a i cs
	toProp = id
	getInfoRecursive (IProperty _ _ i cs) = 
		i <> mconcat (map getInfoRecursive cs)
instance IsProp (Property NoInfo) where
	describe (SProperty _ a cs) d = SProperty d a cs
	toProp = toIProperty
	getInfoRecursive _ = mempty

instance IsProp RevertableProperty where
	-- | Sets the description of both sides.
	describe (RevertableProperty p1 p2) d = 
		RevertableProperty (describe p1 d) (describe p2 ("not " ++ d))
	toProp (RevertableProperty p1 _) = p1
	-- | Return the Info of the currently active side.
	getInfoRecursive (RevertableProperty p1 _p2) = getInfoRecursive p1

class Requires x y r where
	-- Indicates that the first property depends on the second,
	-- so before the first is ensured, the second will be ensured.
	requires :: x -> y -> r

instance Requires (Property HasInfo) (Property HasInfo) (Property HasInfo) where
	requires (IProperty d1 a1 i1 cs1) y@(IProperty _d2 a2 _i2 _cs2) =
		IProperty d1 (a2 `andThen` a1) i1 (y : cs1)

instance Requires (Property HasInfo) (Property NoInfo) (Property HasInfo) where
	requires (IProperty d1 a1 i1 cs1) y@(SProperty _d2 a2 _cs2) =
		IProperty d1 (a2 `andThen` a1) i1 (toIProperty y : cs1)

instance Requires (Property NoInfo) (Property HasInfo) (Property HasInfo) where
	requires x y = requires y x

instance Requires (Property NoInfo) (Property NoInfo) (Property NoInfo) where
	requires (SProperty d1 a1  cs1) y@(SProperty _d2 a2 _cs2) =
		SProperty d1 (a2 `andThen` a1) (y : cs1)

instance Requires RevertableProperty (Property HasInfo) RevertableProperty where
	requires (RevertableProperty p1 p2) y =
		RevertableProperty (p1 `requires` y) p2

instance Requires RevertableProperty (Property NoInfo) RevertableProperty where
	requires (RevertableProperty p1 p2) y =
		RevertableProperty (p1 `requires` toIProperty y) p2

instance Requires RevertableProperty RevertableProperty RevertableProperty where
	requires (RevertableProperty x1 x2) (RevertableProperty y1 y2) =
		RevertableProperty
			(x1 `requires` y1)
			-- when reverting, run actions in reverse order
			(y2 `requires` x2)

andThen :: Propellor Result -> Propellor Result -> Propellor Result
x `andThen` y = do
	r <- x
	case r of
		FailedChange -> return FailedChange
		_ -> y

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

{-# LANGUAGE PackageImports #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Propellor.Types
	( Host(..)
	, Property
	, Info
	, HasInfo
	, NoInfo
	, CInfo
	, Desc
	, infoProperty
	, simpleProperty
	, adjustPropertySatisfy
	, propertyInfo
	, propertyDesc
	, propertyChildren
	, RevertableProperty(..)
	, (<!>)
	, IsProp(..)
	, Combines(..)
	, CombinedType
	, combineWith
	, Propellor(..)
	, EndAction(..)
	, module Propellor.Types.OS
	, module Propellor.Types.Dns
	, module Propellor.Types.Result
	, propertySatisfy
	, ignoreInfo
	) where

import Data.Monoid
import Control.Applicative
import "mtl" Control.Monad.RWS.Strict
import Control.Monad.Catch
import Data.Typeable

import Propellor.Types.Info
import Propellor.Types.OS
import Propellor.Types.Dns
import Propellor.Types.Result

-- | Everything Propellor knows about a system: Its hostname,
-- properties and their collected info.
data Host = Host
	{ hostName :: HostName
	, hostProperties :: [Property HasInfo]
	, hostInfo :: Info
	}
	deriving (Show, Typeable)

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
		, MonadCatch
		, MonadThrow
		, MonadMask
		)

instance Monoid (Propellor Result) where
	mempty = return NoChange
	-- | The second action is only run if the first action does not fail.
	mappend x y = do
		rx <- x
		case rx of
			FailedChange -> return FailedChange
			_ -> do
				ry <- y
				return (rx <> ry)

-- | An action that Propellor runs at the end, after trying to satisfy all
-- properties. It's passed the combined Result of the entire Propellor run.
data EndAction = EndAction Desc (Result -> Propellor Result)

type Desc = String

-- | The core data type of Propellor, this represents a property
-- that the system should have, and an action to ensure it has the
-- property.
--
-- A property can have associated `Info` or not. This is tracked at the
-- type level with Property `NoInfo` and Property `HasInfo`.
--
-- There are many instances and type families, which are mostly used
-- internally, so you needn't worry about them.
data Property i where
	IProperty :: Desc -> Propellor Result -> Info -> [Property HasInfo] -> Property HasInfo
	SProperty :: Desc -> Propellor Result -> [Property NoInfo] -> Property NoInfo

-- | Indicates that a Property has associated Info.
data HasInfo
-- | Indicates that a Property does not have Info.
data NoInfo

-- | Type level calculation of the combination of HasInfo and/or NoInfo
type family CInfo x y
type instance CInfo HasInfo HasInfo = HasInfo
type instance CInfo HasInfo NoInfo = HasInfo
type instance CInfo NoInfo HasInfo = HasInfo
type instance CInfo NoInfo NoInfo = NoInfo

-- | Constructs a Property with associated Info.
infoProperty 
	:: Desc -- ^ description of the property
	-> Propellor Result -- ^ action to run to satisfy the property (must be idempotent; may run repeatedly)
	-> Info -- ^ info associated with the property
	-> [Property i] -- ^ child properties
	-> Property HasInfo
infoProperty d a i cs = IProperty d a i (map toIProperty cs)

-- | Constructs a Property with no Info.
simpleProperty :: Desc -> Propellor Result -> [Property NoInfo] -> Property NoInfo
simpleProperty = SProperty

toIProperty :: Property i -> Property HasInfo
toIProperty p@(IProperty {}) = p
toIProperty (SProperty d s cs) = IProperty d s mempty (map toIProperty cs)

toSProperty :: Property i -> Property NoInfo
toSProperty (IProperty d s _ cs) = SProperty d s (map toSProperty cs)
toSProperty p@(SProperty {}) = p

-- | Makes a version of a Proprty without its Info.
-- Use with caution!
ignoreInfo :: Property i -> Property NoInfo
ignoreInfo = toSProperty

-- | Gets the action that can be run to satisfy a Property.
-- You should never run this action directly. Use
-- 'Propellor.Engine.ensureProperty` instead.
propertySatisfy :: Property i -> Propellor Result
propertySatisfy (IProperty _ a _ _) = a
propertySatisfy (SProperty _ a _) = a

instance Show (Property i) where
        show p = "property " ++ show (propertyDesc p)

-- | Changes the action that is performed to satisfy a property. 
adjustPropertySatisfy :: Property i -> (Propellor Result -> Propellor Result) -> Property i
adjustPropertySatisfy (IProperty d s i cs) f = IProperty d (f s) i cs
adjustPropertySatisfy (SProperty d s cs) f = SProperty d (f s) cs

propertyInfo :: Property i -> Info
propertyInfo (IProperty _ _ i _) = i
propertyInfo (SProperty {}) = mempty

propertyDesc :: Property i -> Desc
propertyDesc (IProperty d _ _ _) = d
propertyDesc (SProperty d _ _) = d

-- | A Property can include a list of child properties that it also
-- satisfies. This allows them to be introspected to collect their info, etc.
propertyChildren :: Property i -> [Property i]
propertyChildren (IProperty _ _ _ cs) = cs
propertyChildren (SProperty _ _ cs) = cs

-- | A property that can be reverted. The first Property is run
-- normally and the second is run when it's reverted.
data RevertableProperty = RevertableProperty (Property HasInfo) (Property HasInfo)

-- | Shorthand to construct a revertable property.
(<!>) :: Property i1 -> Property i2 -> RevertableProperty
p1 <!> p2 = RevertableProperty (toIProperty p1) (toIProperty p2)

-- | Class of types that can be used as properties of a host.
class IsProp p where
	setDesc :: p -> Desc -> p
	toProp :: p -> Property HasInfo
	getDesc :: p -> Desc
	-- | Gets the info of the property, combined with all info
	-- of all children properties.
	getInfoRecursive :: p -> Info

instance IsProp (Property HasInfo) where
	setDesc (IProperty _ a i cs) d = IProperty d a i cs
	toProp = id
	getDesc = propertyDesc
	getInfoRecursive (IProperty _ _ i cs) = 
		i <> mconcat (map getInfoRecursive cs)
instance IsProp (Property NoInfo) where
	setDesc (SProperty _ a cs) d = SProperty d a cs
	toProp = toIProperty
	getDesc = propertyDesc
	getInfoRecursive _ = mempty

instance IsProp RevertableProperty where
	-- | Sets the description of both sides.
	setDesc (RevertableProperty p1 p2) d = 
		RevertableProperty (setDesc p1 d) (setDesc p2 ("not " ++ d))
	getDesc (RevertableProperty p1 _) = getDesc p1
	toProp (RevertableProperty p1 _) = p1
	-- | Return the Info of the currently active side.
	getInfoRecursive (RevertableProperty p1 _p2) = getInfoRecursive p1

-- | Type level calculation of the type that results from combining two
-- types of properties.
type family CombinedType x y
type instance CombinedType (Property x) (Property y) = Property (CInfo x y)
type instance CombinedType RevertableProperty (Property NoInfo) = RevertableProperty
type instance CombinedType RevertableProperty (Property HasInfo) = RevertableProperty
type instance CombinedType RevertableProperty RevertableProperty = RevertableProperty

class Combines x y where
	-- | Combines two properties. The second property is ensured
	-- first, and only once it is successfully ensures will the first
	-- be ensured. The combined property will have the description of
	-- the first property.
	(<<>>) :: x -> y -> CombinedType x y

-- | Combines together two properties, yielding a property that
-- has the description and info of the first, and that has the second
-- property as a child. The two actions to satisfy the properties
-- are passed to a function that can combine them in arbitrary ways.
combineWith
	:: (Combines (Property x) (Property y))
	=> (Propellor Result -> Propellor Result -> Propellor Result)
	-> Property x
	-> Property y
	-> CombinedType (Property x) (Property y)
combineWith f x y = adjustPropertySatisfy (x <<>> y) $ \_ ->
	f (propertySatisfy $ toSProperty x) (propertySatisfy $ toSProperty y)

instance Combines (Property HasInfo) (Property HasInfo) where
	(IProperty d1 a1 i1 cs1) <<>> y@(IProperty _d2 a2 _i2 _cs2) =
		IProperty d1 (a2 <> a1) i1 (y : cs1)

instance Combines (Property HasInfo) (Property NoInfo) where
	(IProperty d1 a1 i1 cs1) <<>> y@(SProperty _d2 a2 _cs2) =
		IProperty d1 (a2 <> a1) i1 (toIProperty y : cs1)

instance Combines (Property NoInfo) (Property HasInfo) where
	(SProperty d1 a1 cs1) <<>> y@(IProperty _d2 a2 _i2 _cs2) =
		IProperty d1 (a2 <> a1) mempty (y : map toIProperty cs1)

instance Combines (Property NoInfo) (Property NoInfo) where
	(SProperty d1 a1  cs1) <<>> y@(SProperty _d2 a2 _cs2) =
		SProperty d1 (a2 <> a1) (y : cs1)

instance Combines RevertableProperty (Property HasInfo) where
	(RevertableProperty p1 p2) <<>> y =
		RevertableProperty (p1 <<>> y) p2

instance Combines RevertableProperty (Property NoInfo) where
	(RevertableProperty p1 p2) <<>> y =
		RevertableProperty (p1 <<>> toIProperty y) p2

instance Combines RevertableProperty RevertableProperty where
	(RevertableProperty x1 x2) <<>> (RevertableProperty y1 y2) =
		RevertableProperty
			(x1 <<>> y1)
			-- when reverting, run actions in reverse order
			(y2 <<>> x2)

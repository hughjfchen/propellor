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
	, MkRevertableProperty(..)
	, IsProp(..)
	, Combines(..)
	, CombinedType
	, ResultCombiner
	, Propellor(..)
	, LiftPropellor(..)
	, EndAction(..)
	, module Propellor.Types.OS
	, module Propellor.Types.Dns
	, module Propellor.Types.Result
	, propertySatisfy
	, ignoreInfo
	) where

import Data.Monoid
import "mtl" Control.Monad.RWS.Strict
import Control.Monad.Catch
import Data.Typeable
import Control.Applicative
import Prelude

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

class LiftPropellor m where
	liftPropellor :: m a -> Propellor a

instance LiftPropellor Propellor where
	liftPropellor = id

instance LiftPropellor IO where
	liftPropellor = liftIO

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

instance Show (Property i) where
        show p = "property " ++ show (propertyDesc p)

-- | A Property can include a list of child properties that it also
-- satisfies. This allows them to be introspected to collect their info, etc.
propertyChildren :: Property i -> [Property i]
propertyChildren (IProperty _ _ _ cs) = cs
propertyChildren (SProperty _ _ cs) = cs

-- | A property that can be reverted. The first Property is run
-- normally and the second is run when it's reverted.
data RevertableProperty i = RevertableProperty
	{ setupRevertableProperty :: Property i
	, undoRevertableProperty :: Property i
	}

instance Show (RevertableProperty i) where
        show (RevertableProperty p _) = show p

class MkRevertableProperty i1 i2 where
	-- | Shorthand to construct a revertable property.
	(<!>) :: Property i1 -> Property i2 -> RevertableProperty (CInfo i1 i2)

instance MkRevertableProperty HasInfo HasInfo where
	x <!> y = RevertableProperty x y
instance MkRevertableProperty NoInfo NoInfo where
	x <!> y = RevertableProperty x y
instance MkRevertableProperty NoInfo HasInfo where
	x <!> y = RevertableProperty (toProp x) y
instance MkRevertableProperty HasInfo NoInfo where
	x <!> y = RevertableProperty x (toProp y)

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

instance IsProp (RevertableProperty HasInfo) where
	setDesc = setDescR
	getDesc (RevertableProperty p1 _) = getDesc p1
	toProp (RevertableProperty p1 _) = p1
	-- | Return the Info of the currently active side.
	getInfoRecursive (RevertableProperty p1 _p2) = getInfoRecursive p1
instance IsProp (RevertableProperty NoInfo) where
	setDesc = setDescR
	getDesc (RevertableProperty p1 _) = getDesc p1
	toProp (RevertableProperty p1 _) = toProp p1
	getInfoRecursive (RevertableProperty _ _) = mempty

-- | Sets the description of both sides.
setDescR :: IsProp (Property i) => RevertableProperty i -> Desc -> RevertableProperty i
setDescR (RevertableProperty p1 p2) d =
	RevertableProperty (setDesc p1 d) (setDesc p2 ("not " ++ d))

-- | Type level calculation of the type that results from combining two
-- types of properties.
type family CombinedType x y
type instance CombinedType (Property x) (Property y) = Property (CInfo x y)
type instance CombinedType (RevertableProperty x) (RevertableProperty y) = RevertableProperty (CInfo x y)
-- When only one of the properties is revertable, the combined property is
-- not fully revertable, so is not a RevertableProperty.
type instance CombinedType (RevertableProperty x) (Property y) = Property (CInfo x y)
type instance CombinedType (Property x) (RevertableProperty y) = Property (CInfo x y)

type ResultCombiner = Propellor Result -> Propellor Result -> Propellor Result

class Combines x y where
	-- | Combines together two properties, yielding a property that
	-- has the description and info of the first, and that has the second
	-- property as a child. 
	combineWith 
		:: ResultCombiner
		-- ^ How to combine the actions to satisfy the properties.
		-> ResultCombiner
		-- ^ Used when combining revertable properties, to combine
		-- their reversion actions.
		-> x
		-> y
		-> CombinedType x y

instance Combines (Property HasInfo) (Property HasInfo) where
	combineWith f _ (IProperty d1 a1 i1 cs1) y@(IProperty _d2 a2 _i2 _cs2) =
		IProperty d1 (f a1 a2) i1 (y : cs1)

instance Combines (Property HasInfo) (Property NoInfo) where
	combineWith f _ (IProperty d1 a1 i1 cs1) y@(SProperty _d2 a2 _cs2) =
		IProperty d1 (f a1 a2) i1 (toIProperty y : cs1)

instance Combines (Property NoInfo) (Property HasInfo) where
	combineWith f _ (SProperty d1 a1 cs1) y@(IProperty _d2 a2 _i2 _cs2) =
		IProperty d1 (f a1 a2) mempty (y : map toIProperty cs1)

instance Combines (Property NoInfo) (Property NoInfo) where
	combineWith f _ (SProperty d1 a1  cs1) y@(SProperty _d2 a2 _cs2) =
		SProperty d1 (f a1 a2) (y : cs1)

instance Combines (RevertableProperty NoInfo) (RevertableProperty NoInfo) where
	combineWith = combineWithRR
instance Combines (RevertableProperty HasInfo) (RevertableProperty HasInfo) where
	combineWith = combineWithRR
instance Combines (RevertableProperty HasInfo) (RevertableProperty NoInfo) where
	combineWith = combineWithRR
instance Combines (RevertableProperty NoInfo) (RevertableProperty HasInfo) where
	combineWith = combineWithRR
instance Combines (RevertableProperty NoInfo) (Property HasInfo) where
	combineWith = combineWithRP
instance Combines (RevertableProperty NoInfo) (Property NoInfo) where
	combineWith = combineWithRP
instance Combines (RevertableProperty HasInfo) (Property HasInfo) where
	combineWith = combineWithRP
instance Combines (RevertableProperty HasInfo) (Property NoInfo) where
	combineWith = combineWithRP
instance Combines (Property HasInfo) (RevertableProperty NoInfo) where
	combineWith = combineWithPR
instance Combines (Property NoInfo) (RevertableProperty NoInfo) where
	combineWith = combineWithPR
instance Combines (Property HasInfo) (RevertableProperty HasInfo) where
	combineWith = combineWithPR
instance Combines (Property NoInfo) (RevertableProperty HasInfo) where
	combineWith = combineWithPR

combineWithRR 
	:: Combines (Property x) (Property y)
	=> ResultCombiner
	-> ResultCombiner
	-> RevertableProperty x
	-> RevertableProperty y
	-> RevertableProperty (CInfo x y)
combineWithRR sf tf (RevertableProperty s1 t1) (RevertableProperty s2 t2) =
	RevertableProperty
		(combineWith sf tf s1 s2)
		(combineWith tf sf t1 t2)

combineWithRP
	:: Combines (Property i) y
	=> (Propellor Result -> Propellor Result -> Propellor Result)
	-> (Propellor Result -> Propellor Result -> Propellor Result)
	-> RevertableProperty i
	-> y
	-> CombinedType (Property i) y
combineWithRP sf tf (RevertableProperty x _) y = combineWith sf tf x y

combineWithPR
	:: Combines x (Property i)
	=> (Propellor Result -> Propellor Result -> Propellor Result)
	-> (Propellor Result -> Propellor Result -> Propellor Result)
	-> x
	-> RevertableProperty i
	-> CombinedType x (Property i)
combineWithPR sf tf x (RevertableProperty y _) = combineWith sf tf x y

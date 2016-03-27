{-# LANGUAGE PackageImports #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Propellor.Types
	( Host(..)
	, Property(..)
	, property
	, Info
	, Desc
	, MetaType(..)
	, MetaTypes
	, OS(..)
	, UnixLike
	, Linux
	, DebianLike
	, Debian
	, Buntish
	, FreeBSD
	, HasInfo
	, type (+)
	, addInfoProperty
	, addInfoProperty'
	, adjustPropertySatisfy
	, RevertableProperty(..)
	, (<!>)
	, ChildProperty
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
	, module Propellor.Types.ZFS
	, TightenTargets(..)
	, SingI
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
import Propellor.Types.MetaTypes
import Propellor.Types.ZFS

-- | Everything Propellor knows about a system: Its hostname,
-- properties and their collected info.
data Host = Host
	{ hostName :: HostName
	, hostProperties :: [ChildProperty]
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
-- that the system should have, with a descrition, an action to ensure
-- it has the property, and perhaps some Info that can be added to Hosts
-- that have the property.
--
-- A property has a list of `[MetaType]`, which is part of its type.
--
-- There are many instances and type families, which are mostly used
-- internally, so you needn't worry about them.
data Property metatypes = Property metatypes Desc (Propellor Result) Info [ChildProperty]

instance Show (Property metatypes) where
	show p = "property " ++ show (getDesc p)

-- | Since there are many different types of Properties, they cannot be put
-- into a list. The simplified ChildProperty can be put into a list.
data ChildProperty = ChildProperty Desc (Propellor Result) Info [ChildProperty]

instance Show ChildProperty where
	show = getDesc

-- | Constructs a Property, from a description and an action to run to
-- ensure the Property is met.
--
-- You can specify any metatypes that make sense to indicate what OS
-- the property targets, etc.
--
-- For example:
--
-- > foo :: Property Debian
-- > foo = mkProperty "foo" (...)
--
-- Note that using this needs LANGUAGE PolyKinds.
property
	:: SingI metatypes
	=> Desc
	-> Propellor Result
	-> Property (MetaTypes metatypes)
property d a = Property sing d a mempty mempty

-- | Adds info to a Property.
--
-- The new Property will include HasInfo in its metatypes.
addInfoProperty
	:: (MetaTypes metatypes' ~ (+) HasInfo metatypes, SingI metatypes')
	=> Property metatypes
	-> Info
	-> Property (MetaTypes metatypes')
addInfoProperty (Property _ d a oldi c) newi =
	Property sing d a (oldi <> newi) c

-- | Adds more info to a Property that already HasInfo.
addInfoProperty'
	:: (IncludesInfo metatypes ~ 'True)
	=> Property metatypes
	-> Info
	-> Property metatypes
addInfoProperty' (Property t d a oldi c) newi =
	Property t d a (oldi <> newi) c

-- | Changes the action that is performed to satisfy a property.
adjustPropertySatisfy :: Property metatypes -> (Propellor Result -> Propellor Result) -> Property metatypes
adjustPropertySatisfy (Property t d s i c) f = Property t d (f s) i c

-- | A property that can be reverted. The first Property is run
-- normally and the second is run when it's reverted.
data RevertableProperty setupmetatypes undometatypes = RevertableProperty
	{ setupRevertableProperty :: Property setupmetatypes
	, undoRevertableProperty :: Property undometatypes
	}

instance Show (RevertableProperty setupmetatypes undometatypes) where
	show (RevertableProperty p _) = show p

-- | Shorthand to construct a revertable property from any two Properties.
(<!>)
	:: Property setupmetatypes
	-> Property undometatypes
	-> RevertableProperty setupmetatypes undometatypes
setup <!> undo = RevertableProperty setup undo

class IsProp p where
	setDesc :: p -> Desc -> p
	getDesc :: p -> Desc
	getChildren :: p -> [ChildProperty]
	addChildren :: p -> [ChildProperty] -> p
	-- | Gets the info of the property, combined with all info
	-- of all children properties.
	getInfoRecursive :: p -> Info
	-- | Info, not including info from children.
	getInfo :: p -> Info
	-- | Gets a ChildProperty representing the Property.
	-- You should not normally need to use this.
	toChildProperty :: p -> ChildProperty
	-- | Gets the action that can be run to satisfy a Property.
	-- You should never run this action directly. Use
	-- 'Propellor.EnsureProperty.ensureProperty` instead.
	getSatisfy :: p -> Propellor Result

instance IsProp (Property metatypes) where
	setDesc (Property t _ a i c) d = Property t d a i c
	getDesc (Property _ d _ _ _) = d
	getChildren (Property _ _ _ _ c) = c
	addChildren (Property t d a i c) c' = Property t d a i (c ++ c')
	getInfoRecursive (Property _ _ _ i c) =
		i <> mconcat (map getInfoRecursive c)
	getInfo (Property _ _ _ i _) = i
	toChildProperty (Property _ d a i c) = ChildProperty d a i c
	getSatisfy (Property _ _ a _ _) = a

instance IsProp ChildProperty where
	setDesc (ChildProperty _ a i c) d = ChildProperty d a i c
	getDesc (ChildProperty d _ _ _) = d
	getChildren (ChildProperty _ _ _ c) = c
	addChildren (ChildProperty d a i c) c' = ChildProperty d a i (c ++ c')
	getInfoRecursive (ChildProperty _ _ i c) =
		i <> mconcat (map getInfoRecursive c)
	getInfo (ChildProperty _ _ i _) = i
	toChildProperty = id
	getSatisfy (ChildProperty _ a _ _) = a

instance IsProp (RevertableProperty setupmetatypes undometatypes) where
	-- | Sets the description of both sides.
	setDesc (RevertableProperty p1 p2) d =
		RevertableProperty (setDesc p1 d) (setDesc p2 ("not " ++ d))
	getDesc (RevertableProperty p1 _) = getDesc p1
	getChildren (RevertableProperty p1 _) = getChildren p1
	-- | Only add children to the active side.
	addChildren (RevertableProperty p1 p2) c = RevertableProperty (addChildren p1 c) p2
	-- | Return the Info of the currently active side.
	getInfoRecursive (RevertableProperty p1 _p2) = getInfoRecursive p1
	getInfo (RevertableProperty p1 _p2) = getInfo p1
	toChildProperty (RevertableProperty p1 _p2) = toChildProperty p1
	getSatisfy (RevertableProperty p1 _) = getSatisfy p1

-- | Type level calculation of the type that results from combining two
-- types of properties.
type family CombinedType x y
type instance CombinedType (Property (MetaTypes x)) (Property (MetaTypes y)) = Property (MetaTypes (Combine x y))
type instance CombinedType (RevertableProperty (MetaTypes x) (MetaTypes x')) (RevertableProperty (MetaTypes y) (MetaTypes y')) = RevertableProperty (MetaTypes (Combine x y)) (MetaTypes (Combine x' y'))
-- When only one of the properties is revertable, the combined property is
-- not fully revertable, so is not a RevertableProperty.
type instance CombinedType (RevertableProperty (MetaTypes x) (MetaTypes x')) (Property (MetaTypes y)) = Property (MetaTypes (Combine x y))
type instance CombinedType (Property (MetaTypes x)) (RevertableProperty (MetaTypes y) (MetaTypes y')) = Property (MetaTypes (Combine x y))

type ResultCombiner = Propellor Result -> Propellor Result -> Propellor Result

class Combines x y where
	-- | Combines together two properties, yielding a property that
	-- has the description and info of the first, and that has the
	-- second property as a child property.
	combineWith
		:: ResultCombiner
		-- ^ How to combine the actions to satisfy the properties.
		-> ResultCombiner
		-- ^ Used when combining revertable properties, to combine
		-- their reversion actions.
		-> x
		-> y
		-> CombinedType x y

instance (CheckCombinable x y ~ 'CanCombine, SingI (Combine x y)) => Combines (Property (MetaTypes x)) (Property (MetaTypes y)) where
	combineWith f _ (Property _ d1 a1 i1 c1) (Property _ d2 a2 i2 c2) =
		Property sing d1 (f a1 a2) i1 (ChildProperty d2 a2 i2 c2 : c1)
instance (CheckCombinable x y ~ 'CanCombine, CheckCombinable x' y' ~ 'CanCombine, SingI (Combine x y), SingI (Combine x' y')) => Combines (RevertableProperty (MetaTypes x) (MetaTypes x')) (RevertableProperty (MetaTypes y) (MetaTypes y')) where
	combineWith sf tf (RevertableProperty s1 t1) (RevertableProperty s2 t2) =
		RevertableProperty
			(combineWith sf tf s1 s2)
			(combineWith tf sf t1 t2)
instance (CheckCombinable x y ~ 'CanCombine, SingI (Combine x y)) => Combines (RevertableProperty (MetaTypes x) (MetaTypes x')) (Property (MetaTypes y)) where
	combineWith sf tf (RevertableProperty x _) y = combineWith sf tf x y
instance (CheckCombinable x y ~ 'CanCombine, SingI (Combine x y)) => Combines (Property (MetaTypes x)) (RevertableProperty (MetaTypes y) (MetaTypes y')) where
	combineWith sf tf x (RevertableProperty y _) = combineWith sf tf x y

class TightenTargets p where
	-- | Tightens the MetaType list of a Property (or similar),
	-- to contain fewer targets.
	--
	-- For example, to make a property that uses apt-get, which is only
	-- available on DebianLike systems:
	--
	-- > upgraded :: Property DebianLike
	-- > upgraded = tightenTargets $ cmdProperty "apt-get" ["upgrade"]
	tightenTargets
		:: 
			-- Note that this uses PolyKinds
			( (Targets untightened `NotSuperset` Targets tightened) ~ 'CanCombine
			, (NonTargets tightened `NotSuperset` NonTargets untightened) ~ 'CanCombine
			, SingI tightened
			)
		=> p (MetaTypes untightened)
		-> p (MetaTypes tightened)

instance TightenTargets Property where
	tightenTargets (Property _ d a i c) = Property sing d a i c

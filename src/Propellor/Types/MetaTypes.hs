{-# LANGUAGE TypeOperators, PolyKinds, DataKinds, TypeFamilies, UndecidableInstances, FlexibleInstances, GADTs #-}

module Propellor.Types.MetaTypes (
	MetaType(..),
	UnixLike,
	Linux,
	DebianLike,
	Debian,
	Buntish,
	ArchLinux,
	FreeBSD,
	HasInfo,
	MetaTypes,
	type (+),
	sing,
	SingI,
	IncludesInfo,
	Targets,
	NonTargets,
	NotSuperset,
	Combine,
	CheckCombine(..),
	CheckCombinable,
	type (&&),
	Not,
	EqT,
	Union,
) where

import Propellor.Types.Singletons
import Propellor.Types.OS

import Data.Type.Bool

data MetaType
	= Targeting TargetOS -- ^ A target OS of a Property
	| WithInfo           -- ^ Indicates that a Property has associated Info
	deriving (Show, Eq, Ord)

-- | Any unix-like system
type UnixLike = MetaTypes
	'[ 'Targeting 'OSDebian
	, 'Targeting 'OSBuntish
	, 'Targeting 'OSArchLinux
	, 'Targeting 'OSFreeBSD
	]

-- | Any linux system
type Linux = MetaTypes
	'[ 'Targeting 'OSDebian
	, 'Targeting 'OSBuntish
	, 'Targeting 'OSArchLinux
	]

-- | Debian and derivatives.
type DebianLike = MetaTypes '[ 'Targeting 'OSDebian, 'Targeting 'OSBuntish ]
type Debian = MetaTypes '[ 'Targeting 'OSDebian ]
type Buntish = MetaTypes '[ 'Targeting 'OSBuntish ]
type FreeBSD = MetaTypes '[ 'Targeting 'OSFreeBSD ]
type ArchLinux = MetaTypes '[ 'Targeting 'OSArchLinux ]

-- | Used to indicate that a Property adds Info to the Host where it's used.
type HasInfo = MetaTypes '[ 'WithInfo ]

type family IncludesInfo t :: Bool where
	IncludesInfo (MetaTypes l) = Elem 'WithInfo l

type MetaTypes = Sing

-- This boilerplate would not be needed if the singletons library were
-- used.
data instance Sing (x :: MetaType) where
	OSDebianS :: Sing ('Targeting 'OSDebian)
	OSBuntishS :: Sing ('Targeting 'OSBuntish)
	OSFreeBSDS :: Sing ('Targeting 'OSFreeBSD)
	OSArchLinuxS :: Sing ('Targeting 'OSArchLinux)
	WithInfoS :: Sing 'WithInfo
instance SingI ('Targeting 'OSDebian) where sing = OSDebianS
instance SingI ('Targeting 'OSBuntish) where sing = OSBuntishS
instance SingI ('Targeting 'OSFreeBSD) where sing = OSFreeBSDS
instance SingI ('Targeting 'OSArchLinux) where sing = OSArchLinuxS
instance SingI 'WithInfo where sing = WithInfoS
instance SingKind ('KProxy :: KProxy MetaType) where
	type DemoteRep ('KProxy :: KProxy MetaType) = MetaType
	fromSing OSDebianS = Targeting OSDebian
	fromSing OSBuntishS = Targeting OSBuntish
	fromSing OSFreeBSDS = Targeting OSFreeBSD
	fromSing OSArchLinuxS = Targeting OSArchLinux
	fromSing WithInfoS = WithInfo

-- | Convenience type operator to combine two `MetaTypes` lists.
--
-- For example:
--
-- > HasInfo + Debian
--
-- Which is shorthand for this type:
--
-- > MetaTypes '[WithInfo, Targeting OSDebian]
type family a + b :: * where
	(MetaTypes a) + (MetaTypes b) = MetaTypes (Concat a b)

type family Concat (list1 :: [a]) (list2 :: [a]) :: [a] where
	Concat '[] bs = bs
	Concat (a ': as) bs = a ': (Concat as bs)

-- | Combine two MetaTypes lists, yielding a list
-- that has targets present in both, and nontargets present in either.
type family Combine (list1 :: [a]) (list2 :: [a]) :: [a] where
	Combine (list1 :: [a]) (list2 :: [a]) =
		(Concat
			(NonTargets list1 `Union` NonTargets list2)
			(Targets list1 `Intersect` Targets list2)
		)

-- | Checks if two MetaTypes lists can be safely combined.
--
-- This should be used anywhere Combine is used, as an additional
-- constraint. For example:
--
-- > foo :: (CheckCombinable x y ~ 'CanCombine) => x -> y -> Combine x y
type family CheckCombinable (list1 :: [a]) (list2 :: [a]) :: CheckCombine where
	-- As a special case, if either list is empty, let it be combined
	-- with the other. This relies on MetaTypes list always containing
	-- at least one target, so can only happen if there's already been
	-- a type error. This special case lets the type checker show only
	-- the original type error, and not an extra error due to a later
	-- CheckCombinable constraint.
	CheckCombinable '[] list2 = 'CanCombine
	CheckCombinable list1 '[] = 'CanCombine
	CheckCombinable (l1 ': list1) (l2 ': list2) =
		CheckCombinable' (Combine (l1 ': list1) (l2 ': list2))
type family CheckCombinable' (combinedlist :: [a]) :: CheckCombine where
	CheckCombinable' '[] = 'CannotCombineTargets
	CheckCombinable' (a ': rest) 
		= If (IsTarget a)
			'CanCombine
			(CheckCombinable' rest)

data CheckCombine = CannotCombineTargets | CanCombine

-- | Every item in the subset must be in the superset.
--
-- The name of this was chosen to make type errors more understandable.
type family NotSuperset (superset :: [a]) (subset :: [a]) :: CheckCombine where
	NotSuperset superset '[] = 'CanCombine
	NotSuperset superset (s ': rest) =
		If (Elem s superset)
			(NotSuperset superset rest)
			'CannotCombineTargets

type family IsTarget (a :: t) :: Bool where
	IsTarget ('Targeting a) = 'True
	IsTarget 'WithInfo = 'False

type family Targets (l :: [a]) :: [a] where
	Targets '[] = '[]
	Targets (x ': xs) =
		If (IsTarget x)
			(x ': Targets xs)
			(Targets xs)

type family NonTargets (l :: [a]) :: [a] where
	NonTargets '[] = '[]
	NonTargets (x ': xs) =
		If (IsTarget x)
			(NonTargets xs)
			(x ': NonTargets xs)

-- | Type level elem
type family Elem (a :: t) (list :: [t]) :: Bool where
	Elem a '[] = 'False
	Elem a (b ': bs) = EqT a b || Elem a bs

-- | Type level union.
type family Union (list1 :: [a]) (list2 :: [a]) :: [a] where
	Union '[] list2 = list2
	Union (a ': rest) list2 =
		If (Elem a list2 || Elem a rest)
			(Union rest list2)
			(a ': Union rest list2)

-- | Type level intersection. Duplicate list items are eliminated.
type family Intersect (list1 :: [a]) (list2 :: [a]) :: [a] where
	Intersect '[] list2 = '[]
	Intersect (a ': rest) list2 = 
		If (Elem a list2 && Not (Elem a rest))
			(a ': Intersect rest list2)
			(Intersect rest list2)

-- | Type level equality of metatypes.
type family EqT (a :: MetaType) (b :: MetaType) where
 	EqT a a = 'True
 	EqT a b = 'False

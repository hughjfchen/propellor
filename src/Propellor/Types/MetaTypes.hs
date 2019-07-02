{-# LANGUAGE TypeOperators, PolyKinds, DataKinds, TypeFamilies, UndecidableInstances, FlexibleInstances, GADTs #-}
{-# LANGUAGE CPP #-}

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
	PrettyPrintMetaTypes,
	IsSubset,
	Combine,
	CheckCombinable,
	CheckCombinableNote,
	type (&&),
	Not,
	EqT,
	Union,
	Intersect,
	Difference,
	IfStuck,
	DelayError,
	DelayErrorFcf,
) where

import Propellor.Types.Singletons
import Propellor.Types.OS

import GHC.TypeLits hiding (type (+))
import Data.Type.Bool

#ifdef WITH_TYPE_ERRORS
import Type.Errors
#else
type family IfStuck (expr :: k) (b :: k1) (c :: k1) :: k1 where
	IfStuck expr b c = c
type family DelayError msg where
	DelayError msg = TypeError msg
type family DelayErrorFcf msg where
	DelayErrorFcf msg = TypeError msg
#endif

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

-- | Checks if two MetaTypes lists can be safly combined;
-- eg they have at least one Target in common.
type family IsCombinable (list1 :: [a]) (list2 :: [a]) :: Bool where
	-- As a special case, if either list is empty or only WithInfo, 
	-- let it be combined with the other. This relies on MetaTypes
	-- list always containing at least one Target, so can only happen
	-- if there's already been a type error. This special case lets the
	-- type checker show only the original type error, and not
	-- subsequent errors due to later CheckCombinable constraints.
	IsCombinable '[] list2 = 'True
	IsCombinable list1 '[] = 'True
	IsCombinable ('WithInfo ': list1) list2 = IsCombinable list1 list2
	IsCombinable list1 ('WithInfo ': list2) = IsCombinable list1 list2
	IsCombinable list1 list2 =
		Not (Null (Combine (Targets list1) (Targets list2)))

-- | This (or CheckCombinableNote) should be used anywhere Combine is used, 
-- as an additional constraint. For example:
--
-- > foo :: (CheckCombinable x y ~ 'True) => x -> y -> Combine x y
type family CheckCombinable (list1 :: [a]) (list2 :: [a]) :: Bool where
	CheckCombinable list1 list2 =
		If (IsCombinable list1 list2)
			'True
			(CannotCombine list1 list2 'Nothing)

-- | Allows providing an additional note.
type family CheckCombinableNote (list1 :: [a]) (list2 :: [a]) (note :: ErrorMessage) :: Bool where
	CheckCombinableNote list1 list2 note =
		If (IsCombinable list1 list2)
			'True
			(CannotCombine list1 list2
				('Just ('Text "(" ':<>: note ':<>: 'Text ")"))
			)

-- Checking IfStuck is to avoid massive and useless error message leaking 
-- type families from this module.
type family CannotCombine (list1 :: [a]) (list2 :: [a]) (note :: Maybe ErrorMessage) :: Bool where
	CannotCombine list1 list2 note = 
		IfStuck list1
			(IfStuck list2
				(DelayError (CannotCombineMessage UnknownType UnknownType UnknownTypeNote))
				(DelayErrorFcf (CannotCombineMessage UnknownType (PrettyPrintMetaTypes list2) UnknownTypeNote))
			)
			(IfStuck list2
				(DelayError (CannotCombineMessage (PrettyPrintMetaTypes list1) UnknownType UnknownTypeNote))
				(DelayErrorFcf (CannotCombineMessage (PrettyPrintMetaTypes list1) (PrettyPrintMetaTypes list2) note))
			)

type family UnknownType :: ErrorMessage where
	UnknownType = 'Text "<unknown>"

type family UnknownTypeNote :: Maybe ErrorMessage where
	UnknownTypeNote = 'Just ('Text "(Property <unknown> is often caused by a Property constructor being passed the wrong number of parameters.)")

type family CannotCombineMessage (a :: ErrorMessage) (b :: ErrorMessage) (note :: Maybe ErrorMessage) :: ErrorMessage where
	CannotCombineMessage a b ('Just note) =
		CannotCombineMessage a b 'Nothing
			':$$: note
	CannotCombineMessage a b 'Nothing =
		'Text "Cannot combine Properties:"
			':$$: ('Text "  Property " ':<>: a)
			':$$: ('Text "  Property " ':<>: b)

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

-- | Pretty-prints a list of MetaTypes for display in a type error message.
type family PrettyPrintMetaTypes (l :: [MetaType]) :: ErrorMessage where
	PrettyPrintMetaTypes '[] = 'Text "<none>"
	PrettyPrintMetaTypes (t ': '[]) = PrettyPrintMetaType t
	PrettyPrintMetaTypes (t ': ts) = 
		PrettyPrintMetaType t ':<>: 'Text " + " ':<>: PrettyPrintMetaTypes ts

type family PrettyPrintMetaType t :: ErrorMessage where
	PrettyPrintMetaType 'WithInfo = 'ShowType HasInfo
	PrettyPrintMetaType ('Targeting 'OSDebian) = 'ShowType Debian
	PrettyPrintMetaType ('Targeting 'OSBuntish) = 'ShowType Buntish
	PrettyPrintMetaType ('Targeting 'OSFreeBSD) = 'ShowType FreeBSD
	PrettyPrintMetaType ('Targeting 'OSArchLinux) = 'ShowType ArchLinux
	PrettyPrintMetaType ('Targeting t) = 'ShowType t

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

-- | Type level difference. Items that are in the first list, but not in
-- the second.
type family Difference (list1 :: [a]) (list2 :: [a]) :: [a] where
	Difference '[] list2 = '[]
	Difference (a ': rest) list2 =
		If (Elem a list2)
			(Difference rest list2)
			(a ': Difference rest list2)

-- | Every item in the subset must be in the superset.
type family IsSubset (subset :: [a]) (superset :: [a]) :: Bool where
	IsSubset '[] superset = 'True
	IsSubset (s ': rest) superset =
		If (Elem s superset)
			(IsSubset rest superset)
			'False

-- | Type level null.
type family Null (list :: [a]) :: Bool where
	Null '[] = 'True
	Null l = 'False

-- | Type level equality of metatypes.
type family EqT (a :: MetaType) (b :: MetaType) where
 	EqT a a = 'True
 	EqT a b = 'False

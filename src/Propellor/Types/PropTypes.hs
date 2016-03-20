{-# LANGUAGE CPP, TypeOperators, PolyKinds, DataKinds, TypeFamilies, UndecidableInstances, FlexibleInstances, GADTs #-}

module Propellor.Types.PropTypes (
	Property(..),
	mkProperty,
	mkProperty',
	PropType(..),
	OS(..),
	UnixLike,
	Debian,
	Buntish,
	FreeBSD,
	HasInfo,
	(+),
	OuterPropTypes,
	ensureProperty,
	tightenTargets,
	pickOS,
	Sing,
	sing,
	SingI,
) where

import GHC.TypeLits (Nat)

-- Older versions of ghc lack this module.
-- #if MIN_VERSION_base(4,8,0)
import Data.Type.Equality
-- #endif

----- DEMO ----------

foo :: Property (HasInfo + FreeBSD)
foo = mkProperty' $ \t -> do
	ensureProperty t jail

-- bar :: Property (Debian + UsesPort 80 + FreeBSD)
-- bar = aptinstall `pickOS` jail

aptinstall :: Property (Debian + UsesPort 80)
aptinstall = mkProperty $ do
	return ()

jail :: Property FreeBSD
jail = mkProperty $ do
	return ()

----- END DEMO ----------

data Property proptypes = Property proptypes (IO ())

mkProperty :: SingI l => IO () -> Property (Sing l)
mkProperty = mkProperty' . const

mkProperty' :: SingI l => (OuterPropTypes l -> IO ()) -> Property (Sing l)
mkProperty' a = 
	let p = Property sing (a (outerPropTypes p))
	in p

data PropType
	= Targeting OS -- ^ A target OS of a Property
	| WithInfo     -- ^ Indicates that a Property has associated Info
	| UsedPort Nat -- ^ Indicates that a network port is used by a Property

data OS
	= OSDebian
	| OSBuntish -- ^ A well-known Debian derivative founded by a space tourist. The actual name of this distribution is not used in Propellor per <http://joeyh.name/blog/entry/trademark_nonsense/>
	| OSFreeBSD
	deriving (Show, Eq)

-- | Any unix-like system
type UnixLike = Sing '[ 'Targeting 'OSDebian, 'Targeting 'OSBuntish, 'Targeting 'OSFreeBSD ]
type Debian = Sing '[ 'Targeting 'OSDebian ]
type Buntish = Sing '[ 'Targeting 'OSBuntish ]
type FreeBSD = Sing '[ 'Targeting 'OSFreeBSD ]

-- | Used to indicate that a Property adds Info to the Host where it's used.
type HasInfo = Sing '[ 'WithInfo ]

-- | Used to indicate that a Property uses a network port
type UsesPort n = Sing '[ 'UsedPort n ]

-- | The data family of singleton types.
data family Sing (x :: k)

-- | A class used to pass singleton values implicitly.
class SingI t where
	sing :: Sing t

-- This boilerplatw would not be needed if the singletons library were
-- used. However, we're targeting too old a version of ghc to use it yet.
data instance Sing (x :: PropType) where
	OSDebianS :: Sing ('Targeting 'OSDebian)
	OSBuntishS :: Sing ('Targeting 'OSBuntish)
	OSFreeBSDS :: Sing ('Targeting 'OSFreeBSD)
	WithInfoS :: Sing 'WithInfo
	WithUsedPortS :: Sing n -> Sing ('UsedPort n)
instance SingI ('Targeting 'OSDebian) where sing = OSDebianS
instance SingI ('Targeting 'OSBuntish) where sing = OSBuntishS
instance SingI ('Targeting 'OSFreeBSD) where sing = OSFreeBSDS
instance SingI 'WithInfo where sing = WithInfoS
instance (SingI n) => SingI ('UsedPort n) where sing = WithUsedPortS sing

data instance Sing (x :: [k]) where
	Nil :: Sing '[]
	Cons :: Sing x -> Sing xs -> Sing (x ': xs)
instance (SingI x, SingI xs) => SingI (x ': xs) where sing = Cons sing sing
instance SingI '[] where sing = Nil

-- FIXME: How to implement sing for Nat?
--
-- Since we don't actually currently use the values of singletons, 
-- getting by with undefined for now.
instance SingI (n :: Nat) where sing = undefined

-- | Convenience type operator to combine two `Sing` lists.
--
-- For example:
--
-- > HasInfo + Debian
--
-- Which is shorthand for this type:
--
-- > Sing '[WithInfo, Targeting OSDebian]
type family a + b :: ab
type instance (Sing a) + (Sing b) = Sing (Concat a b)

type family Concat (list1 :: [a]) (list2 :: [a]) :: [a]
type instance Concat '[] bs = bs
type instance Concat (a ': as) bs = a ': (Concat as bs)

newtype OuterPropTypes l = OuterPropTypes (Sing l)

outerPropTypes :: Property (Sing l) -> OuterPropTypes l
outerPropTypes (Property proptypes _) = OuterPropTypes proptypes

-- | Use `mkProperty''` to get the `OuterPropTypes`. For example:
--
-- > foo = Property Debian
-- > foo = mkProperty' $ \t -> do
-- >	ensureProperty t (aptInstall "foo")
--
-- The type checker will prevent using ensureProperty with a property
-- that does not support the target OSes needed by the OuterPropTypes.
-- In the example above, aptInstall must support Debian.
--
-- The type checker will also prevent using ensureProperty with a property
-- with HasInfo in its PropTypes. Doing so would cause the info associated
-- with the property to be lost.
ensureProperty
	::
		( (Targets inner `NotSuperset` Targets outer) ~ 'CanCombineTargets
		, CannotUseEnsurePropertyWithInfo inner ~ 'True
		)
	=> OuterPropTypes outer
	-> Property (Sing inner)
	-> IO ()
ensureProperty (OuterPropTypes outerproptypes) (Property innerproptypes a) = a

-- The name of this was chosen to make type errors a more understandable.
type family CannotUseEnsurePropertyWithInfo (l :: [a]) :: Bool
type instance CannotUseEnsurePropertyWithInfo '[] = 'True
type instance CannotUseEnsurePropertyWithInfo (t ': ts) = Not (t `EqT` 'WithInfo) && CannotUseEnsurePropertyWithInfo ts

-- | Tightens the PropType list of a Property, to contain fewer targets.
--
-- Anything else in the PropType list is passed through unchanged.
tightenTargets
	:: 
		( combined ~ Concat (NonTargets old) (Intersect (Targets old) (Targets new))
		, CannotCombineTargets old new combined ~ 'CanCombineTargets
		, SingI combined
		)
	=> Sing new
	-> Property (Sing old)
	-> Property (Sing combined)
tightenTargets _ (Property old a) = Property sing a

-- | Picks one of the two input properties to use,
-- depending on the targeted OS.
--
-- If both input properties support the targeted OS, then the
-- first will be used.
pickOS
	::
		( combined ~ Union a b
		, SingI combined
		)
	=> Property (Sing a)
	-> Property (Sing b)
	-> Property (Sing combined)
pickOS a@(Property ta ioa) b@(Property tb iob) = Property sing io
  where
	-- TODO pick with of ioa or iob to use based on final OS of
	-- system being run on.
	io = undefined

data CheckCombineTargets = CannotCombineTargets | CanCombineTargets

-- | Detect intersection of two lists that don't have any common targets.
-- 
-- The name of this was chosen to make type errors a more understandable.
type family CannotCombineTargets (list1 :: [a]) (list2 :: [a]) (listr :: [a]) :: CheckCombineTargets
type instance CannotCombineTargets l1 l2 '[] = 'CannotCombineTargets
type instance CannotCombineTargets l1 l2 (a ': rest) = 'CanCombineTargets

-- | Every item in the subset must be in the superset.
--
-- The name of this was chosen to make type errors a more understandable.
type family NotSuperset (superset :: [a]) (subset :: [a]) :: CheckCombineTargets
type instance NotSuperset superset '[] = 'CanCombineTargets
type instance NotSuperset superset (s ': rest) =
	If (Elem s superset)
		(NotSuperset superset rest)
		'CannotCombineTargets

type family IsTarget (a :: t) :: Bool
type instance IsTarget ('Targeting a) = 'True
type instance IsTarget 'WithInfo = 'False

type family Targets (l :: [a]) :: [a]
type instance Targets '[] = '[]
type instance Targets (x ': xs) =
	If (IsTarget x)
		(x ': Targets xs)
		(Targets xs)

type family NonTargets (l :: [a]) :: [a]
type instance NonTargets '[] = '[]
type instance NonTargets (x ': xs) =
	If (IsTarget x)
		(Targets xs)
		(x ': Targets xs)

-- | Type level elem
type family Elem (a :: t) (list :: [t]) :: Bool
type instance Elem a '[] = 'False
type instance Elem a (b ': bs) = EqT a b || Elem a bs

-- | Type level union.
type family Union (list1 :: [a]) (list2 :: [a]) :: [a]
type instance Union '[] list2 = list2
type instance Union (a ': rest) list2 =
	If (Elem a list2 || Elem a rest)
		(Union rest list2)
		(a ': Union rest list2)

-- | Type level intersection. Duplicate list items are eliminated.
type family Intersect (list1 :: [a]) (list2 :: [a]) :: [a]
type instance Intersect '[] list2 = '[]
type instance Intersect (a ': rest) list2 = 
	If (Elem a list2 && Not (Elem a rest))
		(a ': Intersect rest list2)
		(Intersect rest list2)

-- | Type level equality
--
-- This is a very clumsy implmentation, but it works back to ghc 7.6.
type family EqT (a :: t) (b :: t) :: Bool
type instance EqT ('Targeting a) ('Targeting b)  = EqT a b
type instance EqT 'WithInfo      'WithInfo       = 'True
type instance EqT 'WithInfo      ('Targeting b)  = 'False
type instance EqT ('Targeting a) 'WithInfo       = 'False
type instance EqT 'OSDebian  'OSDebian  = 'True
type instance EqT 'OSBuntish 'OSBuntish = 'True
type instance EqT 'OSFreeBSD 'OSFreeBSD = 'True
type instance EqT 'OSDebian  'OSBuntish = 'False
type instance EqT 'OSDebian  'OSFreeBSD = 'False
type instance EqT 'OSBuntish 'OSDebian  = 'False
type instance EqT 'OSBuntish 'OSFreeBSD = 'False
type instance EqT 'OSFreeBSD 'OSDebian  = 'False
type instance EqT 'OSFreeBSD 'OSBuntish = 'False
-- #if MIN_VERSION_base(4,8,0)
type instance EqT ('UsedPort a) ('UsedPort b) = a == b
-- #else
-- On older ghc, equality testing of type Nats is not implemented.
-- Assume two Nats are equal. This means that type level port conflict
-- detection won't work when using ghc 7.6.3.
--type instance EqT ('UsedPort a) ('UsedPort b) = True
-- #endif
-- More modern version if the combinatiorial explosion gets too bad later:
--
-- type family Eq (a :: PropType) (b :: PropType) where
-- 	Eq a a = True
-- 	Eq a b = False

-- | An equivilant to the following is in Data.Type.Bool in
-- modern versions of ghc, but is included here to support ghc 7.6.
type family If (cond :: Bool) (tru :: a) (fls :: a) :: a
type instance If 'True  tru fls = tru
type instance If 'False tru fls = fls
type family (a :: Bool) || (b :: Bool) :: Bool
type instance    'False || 'False = 'False
type instance    'True  || 'True  = 'True
type instance    'True  || 'False = 'True
type instance    'False || 'True  = 'True
type family (a :: Bool) && (b :: Bool) :: Bool
type instance    'False && 'False = 'False
type instance    'True  && 'True  = 'True
type instance    'True  && 'False = 'False
type instance    'False && 'True  = 'False
type family Not (a :: Bool) :: Bool
type instance Not 'False = 'True
type instance Not 'True = 'False


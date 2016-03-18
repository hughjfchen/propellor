{-# LANGUAGE TypeOperators, PolyKinds, DataKinds, TypeFamilies, UndecidableInstances, AllowAmbiguousTypes, TypeSynonymInstances, FlexibleInstances, ScopedTypeVariables, GADTs #-}

module Propellor.Types.Target (
{-
	Target(..),
	Targeting(..),
	mkProperty,
	mkProperty',
	OuterTarget,
	ensureProperty,
	orProperty,
	target,
	UnixLike,
	unixLike,
	DebianOnly,
	debian,
	BuntishOnly,
	buntish,
	FreeBSDOnly,
	freeBSD,
	unionTargets,
	intersectTarget,
-}
) where

import Network.BSD (HostName)
import Data.Typeable
import Data.String
import Data.List

----- DEMO ----------

-- Intentionally a type error! :)
--foo :: Property (Targeting '[OSDebian, OSFreeBSD])
--foo = Property supportedos $ do
--	ensureProperty supportedos jail
--  where supportedos = unionTargets debian freeBSD

{-
bar :: Property (Targeting '[OSDebian, OSFreeBSD])
bar = aptinstall `orProperty` jail

-}

foo :: Property (HasInfo :+: FreeBSD)
foo = mkProperty' $ \t -> do
	ensureProperty t jail

aptinstall :: Property Debian
aptinstall = mkProperty $ do
	return ()

jail :: Property FreeBSD
jail = mkProperty $ do
	return ()

----- END DEMO ----------

data Property proptypes = Property proptypes (IO ())

mkProperty :: Sing l => IO () -> Property (WithTypes l)
mkProperty = mkProperty' . const

mkProperty' :: Sing l => (OuterPropTypes l -> IO ()) -> Property (WithTypes l)
mkProperty' a = 
	let p = Property sing (a (outerPropTypes p))
	in p

data OS = OSDebian | OSBuntish | OSFreeBSD
	deriving (Show, Eq)

data PropType
	= Targeting OS -- ^ A target OS of a Property
	| WithInfo     -- ^ Indicates that a Property has associated Info
	deriving (Show, Eq)

-- | Any unix-like system
type UnixLike = WithTypes '[Targeting OSDebian, Targeting OSBuntish, Targeting OSFreeBSD]
type Debian = WithTypes '[Targeting OSDebian]
type Buntish = WithTypes '[Targeting OSBuntish]
type FreeBSD = WithTypes '[Targeting OSFreeBSD]

type HasInfo = WithTypes '[WithInfo]

data family WithTypes (x :: k)

class Sing t where
	-- Constructor for a singleton WithTypes list.
	sing :: WithTypes t

data instance WithTypes (x :: [k]) where
	Nil :: WithTypes '[]
	Cons :: WithTypes x -> WithTypes xs -> WithTypes (x ': xs)

instance (Sing x, Sing xs) => Sing (x ': xs) where sing = Cons sing sing
instance Sing '[] where sing = Nil

-- This boilerplatw would not be needed if the singletons library were
-- used. However, we're targeting too old a version of ghc to use it yet.
data instance WithTypes (x :: PropType) where
	OSDebianS :: WithTypes ('Targeting OSDebian)
	OSBuntishS :: WithTypes ('Targeting OSBuntish)
	OSFreeBSDS :: WithTypes ('Targeting OSFreeBSD)
	WithInfoS :: WithTypes 'WithInfo
instance Sing ('Targeting OSDebian) where sing = OSDebianS
instance Sing ('Targeting OSBuntish) where sing = OSBuntishS
instance Sing ('Targeting OSFreeBSD) where sing = OSFreeBSDS
instance Sing 'WithInfo where sing = WithInfoS

-- | Convenience type operator to combine two WithTypes lists.
--
-- For example:
--
-- > HasInfo :+: Debian
--
-- Which is shorthand for this type:
--
-- > WithTypes '[WithInfo, Targeting OSDebian]
type family a :+: b :: ab
type instance (WithTypes a) :+: (WithTypes b) = WithTypes (Concat a b)

type family Concat (list1 :: [a]) (list2 :: [a]) :: [a]
type instance Concat '[] bs = bs
type instance Concat (a ': as) bs = a ': (Concat as bs)

newtype OuterPropTypes l = OuterPropTypes (WithTypes l)

outerPropTypes :: Property (WithTypes l) -> OuterPropTypes l
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
	:: ((Targets inner `NotSuperset` Targets outer) ~ CanCombine, NoInfo inner ~ True)
	=> OuterPropTypes outer
	-> Property (WithTypes inner)
	-> IO ()
ensureProperty (OuterPropTypes outerproptypes) (Property innerproptypes a) = a

type family NoInfo (l :: [a]) :: Bool
type instance NoInfo '[] = 'True
type instance NoInfo (t ': ts) = Not (t `EqT` WithInfo) && NoInfo ts

{-

-- | Changes the target of a property.
--
-- This can only tighten the target list to contain fewer targets.
target 
	:: (combinedtarget ~ IntersectTarget oldtarget newtarget,  CannotCombineTargets oldtarget newtarget combinedtarget ~ CanCombineTargets)
	=> Targeting newtarget
	-> Property (Targeting oldtarget)
	-> Property (Targeting combinedtarget)
target newtarget (Property oldtarget a) =
	Property (intersectTarget oldtarget newtarget) a

-- | Picks one of the two input properties to use,
-- depending on the targeted OS.
--
-- If both input properties support the targeted OS, then the
-- first will be used.
orProperty
	:: Property (Targeting a)
	-> Property (Targeting b)
	-> Property (Targeting (UnionTarget a b))
orProperty a@(Property ta ioa) b@(Property tb iob) =
	Property (unionTargets ta tb) io
  where
	-- TODO pick with of ioa or iob to use based on final OS of
	-- system being run on.
	io = undefined

-- | The union of two lists of Targets.
unionTargets
	:: Targeting l1
	-> Targeting l2
	-> Targeting (UnionTarget l1 l2)
unionTargets (Targeting l1) (Targeting l2) =
	Targeting $ nub $ l1 ++ l2

-- | The intersection between two lists of Targets.
intersectTarget
	:: (r ~ IntersectTarget l1 l2, CannotCombineTargets l1 l2 r ~ CanCombineTargets)
	=> Targeting l1
	-> Targeting l2
	-> Targeting r
intersectTarget (Targeting l1) (Targeting l2) =
	Targeting $ nub $ filter (`elem` l2) l1

-}

data CheckCombine = CannotCombine | CanCombine

{-

-- | Detect intersection of two lists that don't have any common OS.
-- 
-- The name of this was chosen to make type errors a more understandable.
type family CannotCombineTargets (list1 :: [a]) (list2 :: [a]) (listr :: [a]) :: CheckCombine
type instance CannotCombineTargets l1 l2 '[] = 'CannotCombineTargets
type instance CannotCombineTargets l1 l2 (a ': rest) = 'CanCombineTargets

-}

-- | Every item in the subset must be in the superset.
--
-- The name of this was chosen to make type errors a more understandable.
type family NotSuperset (superset :: [a]) (subset :: [a]) :: CheckCombine
type instance NotSuperset superset '[] = 'CanCombine
type instance NotSuperset superset (s ': rest) =
	If (Elem s superset)
		(NotSuperset superset rest)
		'CannotCombine

type family Targets (l :: [a]) :: [a]
type instance Targets '[] = '[]
type instance Targets (x ': xs) =
	If (IsTarget x)
		(x ': Targets xs)
		(Targets xs)

type family IsTarget (a :: t) :: Bool
type instance IsTarget (Targeting a) = True
type instance IsTarget HasInfo = False

{-

-- | Type level intersection of lists of Targets
type family IntersectTarget (list1 :: [a]) (list2 :: [a]) :: [a]
type instance IntersectTarget '[] list2 = '[]
type instance IntersectTarget (a ': rest) list2 = 
	If (ElemTarget a list2 && Not (ElemTarget a rest))
		(a ': IntersectTarget rest list2)
		(IntersectTarget rest list2)

-- | Type level union of lists of Targets
type family UnionTarget (list1 :: [a]) (list2 :: [a]) :: [a]
type instance UnionTarget '[] list2 = list2
type instance UnionTarget (a ': rest) list2 =
	If (ElemTarget a list2 || ElemTarget a rest)
		(UnionTarget rest list2)
		(a ': UnionTarget rest list2)

-}

-- | Type level elem
type family Elem (a :: t) (list :: [t]) :: Bool
type instance Elem a '[] = 'False
type instance Elem a (b ': bs) = EqT a b || Elem a bs

-- | Type level equality
--
-- This is a very clumsy implmentation, but it works back to ghc 7.6.
type family EqT (a :: t) (b :: t) :: Bool
type instance EqT (Targeting a) (Targeting b)  = EqT a b
type instance EqT WithInfo      WithInfo       = 'True
type instance EqT WithInfo      (Targeting b)  = 'False
type instance EqT (Targeting a) WithInfo       = 'False
type instance EqT OSDebian  OSDebian  = 'True
type instance EqT OSBuntish OSBuntish = 'True
type instance EqT OSFreeBSD OSFreeBSD = 'True
type instance EqT OSDebian  OSBuntish = 'False
type instance EqT OSDebian  OSFreeBSD = 'False
type instance EqT OSBuntish OSDebian  = 'False
type instance EqT OSBuntish OSFreeBSD = 'False
type instance EqT OSFreeBSD OSDebian  = 'False
type instance EqT OSFreeBSD OSBuntish = 'False
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


{-# LANGUAGE TypeOperators, PolyKinds, DataKinds, TypeFamilies, UndecidableInstances #-}

module Propellor.Types.Target (
	Target(..),
	Targeting(..),
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
	orProperty,
	ensureProperty,
) where

import Network.BSD (HostName)
import Data.Typeable
import Data.String
import Data.List

data Property target = Property target (IO ())

mkProperty :: IO () -> Property UnixLike
mkProperty a = Property unixLike a

-- | Changes the target of a property.
--
-- This can only tighten the target list to contain fewer targets.
target 
	:: (combinedtarget ~ IntersectTarget oldtarget newtarget,  CannotCombineTargets oldtarget newtarget combinedtarget ~ CanCombineTargets)
	=> Targeting newtarget
	-> Property (Targeting oldtarget)
	-> Property (Targeting combinedtarget)
target newtarget (Property oldtarget a) = Property (intersectTarget oldtarget newtarget) a

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

----- DEMO ----------
-- Intentionally a type error! :)
--foo :: Property (Targeting '[OSDebian, OSFreeBSD])
--foo = Property supportedos $ do
--	ensureProperty supportedos jail
--  where supportedos = unionTargets debian freeBSD

--bar :: Property (Targeting '[OSDebian, OSFreeBSD])
bar = aptinstall `orProperty` jail

aptinstall :: Property DebianOnly
aptinstall = target debian $ mkProperty $ do
	return ()

jail :: Property FreeBSDOnly
jail = target freeBSD $ mkProperty $ do
	return ()
----- END DEMO ----------

-- | A Target system, where a Property is indended to be used.
data Target = OSDebian | OSBuntish | OSFreeBSD
	deriving (Show, Eq)

-- | A type-level and value-level list of targets.
data Targeting (targets :: [Target]) = Targeting [Target]
	deriving (Show, Eq)

type UnixLike = Targeting '[OSDebian, OSBuntish, OSFreeBSD]

unixLike :: UnixLike
unixLike = Targeting [OSDebian, OSBuntish, OSFreeBSD]

type DebianOnly = Targeting '[OSDebian]

debian :: DebianOnly
debian = targeting OSDebian

type BuntishOnly = Targeting '[OSBuntish]

buntish :: BuntishOnly
buntish = targeting OSBuntish

type FreeBSDOnly = Targeting '[OSFreeBSD]

freeBSD :: FreeBSDOnly
freeBSD = targeting OSFreeBSD

targeting :: Target -> Targeting os
targeting o = Targeting [o]

-- The outertarget parameter needs to be passed in from the outer property.
ensureProperty
	:: ((innertarget `NotSupersetTargets` outertarget) ~ CanCombineTargets)
	=> Targeting outertarget
	-> Property (Targeting innertarget)
	-> IO ()
ensureProperty outertarget (Property inneros a) = a

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

data CheckCombineTargets = CannotCombineTargets | CanCombineTargets

-- | Detect intersection of two lists that don't have any common OS.
-- 
-- The name of this was chosen to make type errors a more understandable.
type family CannotCombineTargets (list1 :: [a]) (list2 :: [a]) (listr :: [a]) :: CheckCombineTargets
type instance CannotCombineTargets l1 l2 '[] = 'CannotCombineTargets
type instance CannotCombineTargets l1 l2 (a ': rest) = 'CanCombineTargets

-- | Everything in the subset must be in the superset.
--
-- The name of this was chosen to make type errors a more understandable.
type family NotSupersetTargets (superset :: [a]) (subset :: [a]) :: CheckCombineTargets
type instance NotSupersetTargets superset '[] = 'CanCombineTargets
type instance NotSupersetTargets superset (s ': rest) = 
	If (ElemTarget s superset)
		(NotSupersetTargets superset rest)
		'CannotCombineTargets

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

-- | Type level elem for Target
type family ElemTarget (a :: Target) (list :: [Target]) :: Bool
type instance ElemTarget a '[] = 'False
type instance ElemTarget a (b ': bs) = EqTarget a b || ElemTarget a bs

-- | Type level equality for Target
--
-- This is a very clumsy implmentation, but it works back to ghc 7.6.
type family EqTarget (a :: Target) (b :: Target) :: Bool
type instance EqTarget OSDebian  OSDebian  = 'True
type instance EqTarget OSBuntish OSBuntish = 'True
type instance EqTarget OSFreeBSD OSFreeBSD = 'True
type instance EqTarget OSDebian  OSBuntish = 'False
type instance EqTarget OSDebian  OSFreeBSD = 'False
type instance EqTarget OSBuntish OSDebian  = 'False
type instance EqTarget OSBuntish OSFreeBSD = 'False
type instance EqTarget OSFreeBSD OSDebian  = 'False
type instance EqTarget OSFreeBSD OSBuntish = 'False
-- More modern version if the combinatiorial explosion gets too bad later:
--
-- type family EqTarget (a :: Target) (b :: Target) where
-- 	EqTarget a a = True
-- 	EqTarget a b = False

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

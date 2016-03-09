{-# LANGUAGE TypeOperators, PolyKinds, DataKinds, TypeFamilies, UndecidableInstances #-}

module Propellor.Types.Target (
	Target(..),
	Targeting(..),
	debian,
	buntish,
	freeBSD,
	unixlike,
	includeTarget,
	intersectTarget,
) where

import Network.BSD (HostName)
import Data.Typeable
import Data.String

----- DEMO ----------
data Property os = Property os (IO ())

mkProperty :: os -> IO () -> Property os
mkProperty os a = Property os a

-- Intentionally a type error! :)
--foo :: Property (Targeting '[OSDebian, OSFreeBSD])
--foo = mkProperty supportedos $ do
--	ensureProperty supportedos jail
--  where supportedos = includeTarget debian

jail :: Property (Targeting '[OSFreeBSD])
jail = Property freeBSD $ do
	return ()
----- END DEMO ----------

-- | A Target system, where a Property is indended to be used.
data Target = OSDebian | OSBuntish | OSFreeBSD
	deriving (Show, Eq)

-- | A type-level and value-level list of targets.
data Targeting (os :: [Target]) = Targeting [Target]
	deriving (Show, Eq)

-- | Any unix-like OS.
unixlike :: Targeting '[OSDebian, OSBuntish, OSFreeBSD]
unixlike = Targeting [OSDebian, OSBuntish, OSFreeBSD]

debian :: Targeting '[OSDebian]
debian = typeOS OSDebian

buntish :: Targeting '[OSBuntish]
buntish = typeOS OSBuntish

freeBSD :: Targeting '[OSFreeBSD]
freeBSD = typeOS OSFreeBSD

typeOS :: Target -> Targeting os
typeOS o = Targeting [o]

ensureProperty
	:: (CannotCombineTargets outertarget innertarget (IntersectTargeting outertarget innertarget) ~ CanCombineTargets)
	=> Targeting outertarget
	-> Property (Targeting innertarget)
	-> IO ()
ensureProperty outeros (Property inneros a) = a

-- | Adds to a list of targets.
includeTarget
	:: (r ~ ConcatTargeting l1 l2)
	=> Targeting l1
	-> Targeting l2
	-> Targeting r
includeTarget (Targeting l1) (Targeting l2) = Targeting (l1 ++ l2)

-- | Type level concat for Targeting.
type family ConcatTargeting (list1 :: [a]) (list2 :: [a]) :: [a]
type instance ConcatTargeting '[] list2 = list2
type instance ConcatTargeting (a ': rest) list2 = a ': ConcatTargeting rest list2

-- | The intersection between two lists of Targets.
intersectTarget
	:: (r ~ IntersectTargeting l1 l2, CannotCombineTargets l1 l2 r ~ CanCombineTargets)
	=> Targeting l1
	-> Targeting l2
	-> Targeting r
intersectTarget (Targeting l1) (Targeting l2) = Targeting (filter (`elem` l2) l1)

-- | Detect intersection of two lists that don't have any common OS.
-- 
-- The name of this was chosen to make type errors a more understandable.
type family CannotCombineTargets (list1 :: [a]) (list2 :: [a]) (listr :: [a]) :: CheckIntersection
type instance CannotCombineTargets l1 l2 '[] = 'CannotCombineTargets
type instance CannotCombineTargets l1 l2 (a ': rest) = 'CanCombineTargets
data CheckIntersection = CannotCombineTargets | CanCombineTargets

-- | Type level intersection for Targeting
type family IntersectTargeting (list1 :: [a]) (list2 :: [a]) :: [a]
type instance IntersectTargeting '[] list2 = '[]
type instance IntersectTargeting (a ': rest) list2 = 
	If (ElemTargeting a list2)
		(a ': IntersectTargeting rest list2)
		(IntersectTargeting rest list2)

-- | Type level elem for Targeting
type family ElemTargeting (a :: Target) (list :: [Target]) :: Bool
type instance ElemTargeting a '[] = 'False
type instance ElemTargeting a (b ': bs) = EqTarget a b || ElemTargeting a bs

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

-- | This is in Data.Type.Bool with modern versions of ghc, but is included
-- here to support ghc 7.6.
type family If (cond :: Bool) (tru :: a) (fls :: a) :: a
type instance If 'True  tru fls = tru
type instance If 'False tru fls = fls
type family (a :: Bool) || (b :: Bool) :: Bool
type instance 'False || 'False = 'False
type instance 'True  || 'True  = 'True
type instance 'True  || 'False = 'True
type instance 'False || 'True  = 'True

{-# LANGUAGE TypeOperators, PolyKinds, DataKinds, TypeFamilies, UndecidableInstances #-}

module Propellor.Types.OS.TypeLevel (
	SupportedOS(..),
	OSList(..),
	debian,
	buntish,
	freeBSD,
	unixlike,
	includeSupportedOS,
	intersectSupportedOS,
) where

import Network.BSD (HostName)
import Data.Typeable
import Data.String

-- | A supported operating system.
data SupportedOS = OSDebian | OSBuntish | OSFreeBSD
	deriving (Show, Eq)

-- | A type-level and value-level list of supported operating systems.
--
-- If the list is empty, no operating system is supported.
data OSList (os :: [SupportedOS]) = OSList [SupportedOS]
	deriving (Show, Eq)

-- | Any unix-like OS.
unixlike :: OSList '[OSDebian, OSBuntish, OSFreeBSD]
unixlike = OSList [OSDebian, OSBuntish, OSFreeBSD]

debian :: OSList '[OSDebian]
debian = typeOS OSDebian

buntish :: OSList '[OSBuntish]
buntish = typeOS OSBuntish

freeBSD :: OSList '[OSFreeBSD]
freeBSD = typeOS OSFreeBSD

typeOS :: SupportedOS -> OSList os
typeOS o = OSList [o]

data Property os = Property os (IO ())

mkProperty :: os -> IO () -> Property os
mkProperty os a = Property os a

-- Intentionally a type error! :)
--foo :: Property (OSList '[OSDebian, OSFreeBSD])
--foo = mkProperty supportedos $ do
--	ensureProperty supportedos jail
--  where supportedos = includeSupportedOS debian

jail :: Property (OSList '[OSFreeBSD])
jail = Property freeBSD $ do
	return ()

ensureProperty
	:: (CannotCombineOS outeros inneros (IntersectOSList outeros inneros) ~ CanCombineOS)
	=> OSList outeros
	-> Property (OSList inneros)
	-> IO ()
ensureProperty outeros (Property inneros a) = a

-- | Adds to a list of supported OS's.
includeSupportedOS
	:: (r ~ ConcatOSList l1 l2)
	=> OSList l1
	-> OSList l2
	-> OSList r
includeSupportedOS (OSList l1) (OSList l2) = OSList (l1 ++ l2)

-- | Type level concat for OSList.
type family ConcatOSList (list1 :: [a]) (list2 :: [a]) :: [a]
type instance ConcatOSList '[] list2 = list2
type instance ConcatOSList (a ': rest) list2 = a ': ConcatOSList rest list2

-- | The intersection between two lists of supported OS's.
intersectSupportedOS
	:: (r ~ IntersectOSList l1 l2, CannotCombineOS l1 l2 r ~ CanCombineOS)
	=> OSList l1
	-> OSList l2
	-> OSList r
intersectSupportedOS (OSList l1) (OSList l2) = OSList (filter (`elem` l2) l1)

-- | Detect intersection of two lists that don't have any common OS.
-- 
-- The name of this was chosen to make type errors a more understandable.
type family CannotCombineOS (list1 :: [a]) (list2 :: [a]) (listr :: [a]) :: CheckIntersection
type instance CannotCombineOS l1 l2 '[] = 'CannotCombineOS
type instance CannotCombineOS l1 l2 (a ': rest) = 'CanCombineOS
data CheckIntersection = CannotCombineOS | CanCombineOS

-- | Type level intersection for OSList
type family IntersectOSList (list1 :: [a]) (list2 :: [a]) :: [a]
type instance IntersectOSList '[] list2 = '[]
type instance IntersectOSList (a ': rest) list2 = 
	If (ElemOSList a list2)
		(a ': IntersectOSList rest list2)
		(IntersectOSList rest list2)

-- | Type level elem for OSList
type family ElemOSList (a :: SupportedOS) (list :: [SupportedOS]) :: Bool
type instance ElemOSList a '[] = 'False
type instance ElemOSList a (b ': bs) = EqOS a b || ElemOSList a bs

-- | Type level equality for SupportedOS
--
-- This is a very clumsy implmentation, but it works back to ghc 7.6.
type family EqOS (a :: SupportedOS) (b :: SupportedOS) :: Bool
type instance EqOS OSDebian  OSDebian  = 'True
type instance EqOS OSBuntish OSBuntish = 'True
type instance EqOS OSFreeBSD OSFreeBSD = 'True
type instance EqOS OSDebian  OSBuntish = 'False
type instance EqOS OSDebian  OSFreeBSD = 'False
type instance EqOS OSBuntish OSDebian  = 'False
type instance EqOS OSBuntish OSFreeBSD = 'False
type instance EqOS OSFreeBSD OSDebian  = 'False
type instance EqOS OSFreeBSD OSBuntish = 'False
-- More modern version if the combinatiorial explosion gets too bad later:
--
-- type family EqOS (a :: SupportedOS) (b :: SupportedOS) where
-- 	EqOS a a = True
-- 	EqOS a b = False

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

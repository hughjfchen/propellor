{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}

module Propellor.PropAccum
	( host
	, Props(..)
	, props
	, (&)
	, (&^)
	, (!)
	--, propagateContainer
	) where

import Propellor.Types
import Propellor.Types.MetaTypes
import Propellor.Property

import Data.Monoid
import Prelude

-- | Defines a host and its properties.
--
-- > host "example.com" $ props
-- > 	& someproperty
-- > 	! oldproperty
-- > 	& otherproperty
host :: HostName -> Props metatypes -> Host
host hn (Props ps) = Host hn ps (mconcat (map getInfoRecursive ps))

-- | Props is a combination of a list of properties, with their combined 
-- metatypes.
data Props metatypes = Props [ChildProperty]

-- | Start accumulating a list of properties.
--
-- Properties can be added to it using `(&)` etc.
props :: Props UnixLike
props = Props []

infixl 1 &
infixl 1 &^
infixl 1 !

type family GetMetaTypes x
type instance GetMetaTypes (Property (MetaTypes t)) = MetaTypes t
type instance GetMetaTypes (RevertableProperty (MetaTypes t) undo) = MetaTypes t

-- | Adds a property to a Props.
--
-- Can add Properties and RevertableProperties
(&)
	::
		( IsProp p
		, MetaTypes y ~ GetMetaTypes p
		, CheckCombinable x y ~ 'CanCombine
		)
	=> Props (MetaTypes x)
	-> p
	-> Props (MetaTypes (Combine x y))
Props c & p = Props (c ++ [toChildProperty p])

-- | Adds a property before any other properties.
(&^)
	::
		( IsProp p
		, MetaTypes y ~ GetMetaTypes p
		, CheckCombinable x y ~ 'CanCombine
		)
	=> Props (MetaTypes x)
	-> p
	-> Props (MetaTypes (Combine x y))
Props c &^ p = Props (toChildProperty p : c)

-- | Adds a property in reverted form.
(!)
	:: (CheckCombinable x z ~ 'CanCombine)
	=> Props (MetaTypes x)
	-> RevertableProperty (MetaTypes y) (MetaTypes z)
	-> Props (MetaTypes (Combine x z))
Props c ! p = Props (c ++ [toChildProperty (revert p)])

{-

-- | Adjust the provided Property, adding to its
-- propertyChidren the properties of the provided container.
-- 
-- The Info of the propertyChildren is adjusted to only include 
-- info that should be propagated out to the Property.
--
-- Any PrivInfo that uses HostContext is adjusted to use the name
-- of the container as its context.
propagateContainer
	:: (PropAccum container)
	=> String
	-> container
	-> Property metatypes
	-> Property metatypes
propagateContainer containername c prop = Property
	undefined
	(propertyDesc prop)
	(getSatisfy prop)
	(propertyInfo prop)
	(propertyChildren prop ++ hostprops)
  where
	hostprops = map go $ getProperties c
	go p = 
		let i = mapInfo (forceHostContext containername)
			(propagatableInfo (propertyInfo p))
		    cs = map go (propertyChildren p)
		in infoProperty (propertyDesc p) (getSatisfy p) i cs

-}

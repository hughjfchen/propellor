{-# LANGUAGE PackageImports #-}

module Propellor.PropAccum
	( host
	, PropAccum(..)
	, (&)
	, (&^)
	, (!)
	, propigateContainer
	) where

import Data.Monoid

import Propellor.Types
import Propellor.Property
import Propellor.Types.Info
import Propellor.PrivData

-- | Starts accumulating the properties of a Host.
--
-- > host "example.com"
-- > 	& someproperty
-- > 	! oldproperty
-- > 	& otherproperty
host :: HostName -> Host
host hn = Host hn [] mempty

-- | Something that can accumulate properties.
class PropAccum h where
	-- | Adds a property.
	addProp :: IsProp p => h -> p -> h

	-- | Like addProp, but adds the property at the front of the list.
	addPropFront :: IsProp p => h -> p -> h

	getProperties :: h -> [Property HasInfo]

-- | Adds a property to a `Host` or other `PropAccum`
--
-- Can add Properties and RevertableProperties
(&) :: (PropAccum h, IsProp p) => h -> p -> h
(&) = addProp

-- | Adds a property before any other properties.
(&^) :: (PropAccum h, IsProp p) => h -> p -> h
(&^) = addPropFront

-- | Adds a property in reverted form.
(!) :: PropAccum h => h -> RevertableProperty -> h
h ! p = h & revert p

infixl 1 &
infixl 1 &^
infixl 1 !

instance PropAccum Host where
	(Host hn ps is) `addProp`  p = Host hn (ps ++ [toProp p])
		(is <> getInfoRecursive p)
	(Host hn ps is) `addPropFront` p = Host hn (toProp p : ps)
		(getInfoRecursive p <> is)
	getProperties = hostProperties

-- | Adjust the provided Property, adding to its
-- propertyChidren the properties of the provided container.
-- 
-- The Info of the propertyChildren is adjusted to only include 
-- info that should be propigated out to the Property.
--
-- Any PrivInfo that uses HostContext is adjusted to use the name
-- of the container as its context.
propigateContainer
	:: (PropAccum container)
	=> String
	-> container
	-> Property HasInfo
	-> Property HasInfo
propigateContainer containername c prop = infoProperty
	(propertyDesc prop)
	(propertySatisfy prop)
	(propertyInfo prop)
	(propertyChildren prop ++ hostprops)
  where
	hostprops = map go $ getProperties c
	go p = 
		let i = mapInfo (forceHostContext containername)
			(propigatableInfo (propertyInfo p))
		    cs = map go (propertyChildren p)
		in infoProperty (propertyDesc p) (propertySatisfy p) i cs

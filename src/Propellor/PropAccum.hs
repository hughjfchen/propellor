{-# LANGUAGE PackageImports #-}

module Propellor.PropAccum where

import Data.Monoid

import Propellor.Types
import Propellor.Property

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
	--
	-- Can add Properties and RevertableProperties
	(&) :: IsProp p => h -> p -> h

	-- | Like (&), but adds the property as the
	-- first property of the host. Normally, property
	-- order should not matter, but this is useful
	-- when it does.
	(&^) :: IsProp p => h -> p -> h

	getProperties :: h -> [Property]

instance PropAccum Host where
	(Host hn ps is) &  p = Host hn (ps ++ [toProp p])
		(is <> getInfoRecursive p)
	(Host hn ps is) &^ p = Host hn ([toProp p] ++ ps)
		(getInfoRecursive p <> is)
	getProperties = hostProperties

-- | Adds a property in reverted form.
(!) :: PropAccum h => h -> RevertableProperty -> h
h ! p = h & revert p

infixl 1 &^
infixl 1 &
infixl 1 !

-- | Adjust the provided Property, adding to its
-- propertyChidren the properties of the provided container.

-- The Info of the propertyChildren is adjusted to only include 
-- info that should be propigated out to the Property.
--
-- DNS Info is propigated, so that eg, aliases of a PropAccum
-- are reflected in the dns for the host where it runs.
--
-- PrivData Info is propigated, so that properties used inside a
-- PropAccum will have the necessary PrivData available.
propigateContainer :: PropAccum container => container -> Property -> Property
propigateContainer c prop = prop
	{ propertyChildren = propertyChildren prop ++ hostprops
	}
  where
	hostprops = map go $ getProperties c
	go p = 
		let i = propertyInfo p
		in p
			{ propertyInfo = mempty
				{ _dns = _dns i
				, _privData = _privData i
				}
			, propertyChildren = map go (propertyChildren p)
			}

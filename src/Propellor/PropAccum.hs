{-# LANGUAGE PackageImports #-}

module Propellor.PropAccum
	( host
	, props
	, PropAccum(..)
	, (!)
	, PropList
	, propigateContainer
	) where

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

-- | Starts accumulating a list of properties.
--
-- > propertyList "foo" $ props
-- > 	& someproperty
-- > 	! oldproperty
-- > 	& otherproperty
props :: PropList
props = PropList []

-- | Something that can accumulate properties.
class PropAccum h where
	-- | Adds a property.
	--
	-- Can add Properties and RevertableProperties
	(&) :: IsProp p => h -> p -> h

	-- | Like (&), but adds the property at the front of the list.
	(&^) :: IsProp p => h -> p -> h

	getProperties :: h -> [Property HasInfo]

instance PropAccum Host where
	(Host hn ps is) &  p = Host hn (ps ++ [toProp p])
		(is <> getInfoRecursive p)
	(Host hn ps is) &^ p = Host hn ([toProp p] ++ ps)
		(getInfoRecursive p <> is)
	getProperties = hostProperties

data PropList = PropList [Property HasInfo]

instance PropAccum PropList where
	PropList l &  p = PropList (toProp p : l)
	PropList l &^ p = PropList (l ++ [toProp p])
	getProperties (PropList l) = reverse l

-- | Adds a property in reverted form.
(!) :: PropAccum h => h -> RevertableProperty -> h
h ! p = h & revert p

infixl 1 &^
infixl 1 &
infixl 1 !

-- | Adjust the provided Property, adding to its
-- propertyChidren the properties of the provided container.
-- 
-- The Info of the propertyChildren is adjusted to only include 
-- info that should be propigated out to the Property.
--
-- DNS Info is propigated, so that eg, aliases of a PropAccum
-- are reflected in the dns for the host where it runs.
--
-- PrivData Info is propigated, so that properties used inside a
-- PropAccum will have the necessary PrivData available.
propigateContainer
	:: (PropAccum container)
	=> container
	-> Property HasInfo
	-> Property HasInfo
propigateContainer c prop = infoProperty
	(propertyDesc prop)
	(propertySatisfy prop)
	(propertyInfo prop)
	(propertyChildren prop ++ hostprops)
  where
	hostprops = map go $ getProperties c
	go p = 
		let i = propertyInfo p
		    i' = mempty
			{ _dns = _dns i
			, _privData = _privData i
			}
		    cs = map go (propertyChildren p)
		in infoProperty (propertyDesc p) (propertySatisfy p) i' cs

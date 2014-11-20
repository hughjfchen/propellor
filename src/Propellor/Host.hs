{-# LANGUAGE PackageImports #-}

module Propellor.Host where

import Data.Monoid
import qualified Data.Set as S

import Propellor.Types
import Propellor.Info
import Propellor.Property
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
class Hostlike h where
	-- | Adds a property.
	--
	-- Can add Properties and RevertableProperties
	(&) :: IsProp p => h -> p -> h

	-- | Like (&), but adds the property as the
	-- first property of the host. Normally, property
	-- order should not matter, but this is useful
	-- when it does.
	(&^) :: IsProp p => h -> p -> h

	getHost :: h -> Host

instance Hostlike Host where
	(Host hn ps is) &  p = Host hn (ps ++ [toProp p]) (is <> getInfo p)
	(Host hn ps is) &^ p = Host hn ([toProp p] ++ ps) (getInfo p <> is)
	getHost h = h

-- | Adds a property in reverted form.
(!) :: Hostlike h => h -> RevertableProperty -> h
h ! p = h & revert p

infixl 1 &^
infixl 1 &
infixl 1 !

-- | When eg, docking a container, some of the Info about the container
-- should propigate out to the Host it's on. This includes DNS info,
-- so that eg, aliases of the container are reflected in the dns for the
-- host where it runs.
--
-- This adjusts the Property that docks a container, to include such info
-- from the container.
propigateInfo :: Hostlike hl => hl -> Property -> (Info -> Info) -> Property
propigateInfo hl p f = combineProperties (propertyDesc p) $
	p' : dnsprops ++ privprops
  where
	p' = p { propertyInfo = f (propertyInfo p) }
	i = hostInfo (getHost hl)
	dnsprops = map addDNS (S.toList $ _dns i)
	privprops = map addPrivDataField (S.toList $ _privDataFields i)

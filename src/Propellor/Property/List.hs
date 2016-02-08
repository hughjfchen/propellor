{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

module Propellor.Property.List (
	props,
	PropertyList(..),
	PropertyListType,
	PropList(..),
) where

import Propellor.Types
import Propellor.Engine
import Propellor.PropAccum

import Data.Monoid

-- | Starts accumulating a list of properties.
--
-- > propertyList "foo" $ props
-- > 	& someproperty
-- > 	! oldproperty
-- > 	& otherproperty
props :: PropList
props = PropList []

data PropList = PropList [Property HasInfo]

instance PropAccum PropList where
	PropList l `addProp` p = PropList (toProp p : l)
	PropList l `addPropFront` p = PropList (l ++ [toProp p])
	getProperties (PropList l) = reverse l

class PropertyList l where
	-- | Combines a list of properties, resulting in a single property
	-- that when run will run each property in the list in turn,
	-- and print out the description of each as it's run. Does not stop
	-- on failure; does propagate overall success/failure.
	--
	-- Note that Property HasInfo and Property NoInfo are not the same
	-- type, and so cannot be mixed in a list. To make a list of
	-- mixed types, which can also include RevertableProperty,
	-- use `props`
	propertyList :: Desc -> l -> Property (PropertyListType l)

	-- | Combines a list of properties, resulting in one property that
	-- ensures each in turn. Stops if a property fails.
	combineProperties :: Desc -> l -> Property (PropertyListType l)

-- | Type level function to calculate whether a PropertyList has Info.
type family PropertyListType t
type instance PropertyListType [Property HasInfo] = HasInfo
type instance PropertyListType [Property NoInfo] = NoInfo
type instance PropertyListType [RevertableProperty NoInfo] = NoInfo
type instance PropertyListType [RevertableProperty HasInfo] = HasInfo
type instance PropertyListType PropList = HasInfo

instance PropertyList [Property NoInfo] where
	propertyList desc ps = simpleProperty desc (ensureProperties ps) ps
	combineProperties desc ps = simpleProperty desc (combineSatisfy ps NoChange) ps

instance PropertyList [Property HasInfo] where
	-- It's ok to use ignoreInfo here, because the ps are made the
	-- child properties of the property, and so their info is visible
	-- that way.
	propertyList desc ps = infoProperty desc (ensureProperties $ map ignoreInfo ps) mempty ps
	combineProperties desc ps = infoProperty desc (combineSatisfy ps NoChange) mempty ps

instance PropertyList [RevertableProperty HasInfo] where
	propertyList desc ps = propertyList desc (map setupRevertableProperty ps)
	combineProperties desc ps = combineProperties desc (map setupRevertableProperty ps)

instance PropertyList [RevertableProperty NoInfo] where
	propertyList desc ps = propertyList desc (map setupRevertableProperty ps)
	combineProperties desc ps = combineProperties desc (map setupRevertableProperty ps)

instance PropertyList PropList where
	propertyList desc = propertyList desc . getProperties
	combineProperties desc = combineProperties desc . getProperties

combineSatisfy :: [Property i] -> Result -> Propellor Result
combineSatisfy [] rs = return rs
combineSatisfy (l:ls) rs = do
	r <- ensureProperty $ ignoreInfo l
	case r of
		FailedChange -> return FailedChange
		_ -> combineSatisfy ls (r <> rs)

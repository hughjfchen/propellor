{-# LANGUAGE DataKinds, TypeFamilies #-}

module Propellor.Container where

import Propellor.Types
import Propellor.Types.MetaTypes
import Propellor.Types.Info
import Propellor.PrivData

class Container c where
	containerProperties :: c -> [ChildProperty]
	containerInfo :: c -> Info

instance Container Host where
	 containerProperties = hostProperties
	 containerInfo = hostInfo

-- | Adjust the provided Property, adding to its
-- propertyChidren the properties of the provided container.
-- 
-- The Info of the propertyChildren is adjusted to only include 
-- info that should be propagated out to the Property.
--
-- Any PrivInfo that uses HostContext is adjusted to use the name
-- of the container as its context.
propagateContainer
	::
		-- Since the children being added probably have info,
		-- require the Property's metatypes to have info.
		( IncludesInfo metatypes ~ 'True
		, Container c
		)
	=> String
	-> c
	-> Property metatypes
	-> Property metatypes
propagateContainer containername c prop = prop
	`addChildren` map convert (containerProperties c)
  where
	convert p = 
		let n = property (getDesc p) (getSatisfy p) :: Property UnixLike
		    n' = n
		    	`addInfoProperty` mapInfo (forceHostContext containername)
				(propagatableInfo (getInfo p))
		   	`addChildren` map convert (getChildren p)
		in toChildProperty n'

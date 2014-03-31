module Propellor.Types where

import Data.Monoid

type HostName = String
type UserName = String

data Property = Property
	{ propertyDesc :: Desc
	-- | must be idempotent; may run repeatedly
	, propertySatisfy :: IO Result
	}

type Desc = String

data Result = NoChange | MadeChange | FailedChange
	deriving (Show, Eq)

instance Monoid Result where
	mempty = NoChange

	mappend FailedChange _ = FailedChange
	mappend _ FailedChange = FailedChange
	mappend MadeChange _ = MadeChange
	mappend _ MadeChange = MadeChange
	mappend NoChange NoChange = NoChange

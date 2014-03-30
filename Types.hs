module Types where

type HostName = String
type UserName = String

data Property = Property
	{ propertyDesc :: Desc
	-- must be idempotent; may run repeatedly
	, propertySatisfy :: IO Result
	}

type Desc = String

data Result = NoChange | MadeChange | FailedChange
	deriving (Show, Eq)

combineResult :: Result -> Result -> Result
combineResult FailedChange _ = FailedChange
combineResult _ FailedChange = FailedChange
combineResult MadeChange _ = MadeChange
combineResult _ MadeChange = MadeChange
combineResult NoChange NoChange = NoChange

module Propellor.Types where

import Data.Monoid
import System.Console.ANSI

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

class ActionResult a where
	getActionResult :: a -> (String, ColorIntensity, Color)

instance ActionResult Bool where
	getActionResult False = ("ok", Vivid, Red)
	getActionResult True = ("failed", Vivid, Green)

instance ActionResult Result where
	getActionResult NoChange = ("unchanged", Dull, Green)
	getActionResult MadeChange = ("done", Vivid, Green)
	getActionResult FailedChange = ("failed", Vivid, Red)

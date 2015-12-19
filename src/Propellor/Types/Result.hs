module Propellor.Types.Result where

import System.Console.ANSI
import Data.Monoid
import Prelude

-- | There can be three results of satisfying a Property.
data Result = NoChange | MadeChange | FailedChange
	deriving (Read, Show, Eq)

instance Monoid Result where
	mempty = NoChange

	mappend FailedChange _ = FailedChange
	mappend _ FailedChange = FailedChange
	mappend MadeChange _ = MadeChange
	mappend _ MadeChange = MadeChange
	mappend NoChange NoChange = NoChange

class ToResult t where
	toResult :: t -> Result

instance ToResult Bool where
	toResult False = FailedChange
	toResult True = MadeChange

-- | Results of actions, with color.
class ActionResult a where
	getActionResult :: a -> (String, ColorIntensity, Color)

instance ActionResult Bool where
	getActionResult False = ("failed", Vivid, Red)
	getActionResult True = ("done", Dull, Green)

instance ActionResult Result where
	getActionResult NoChange = ("ok", Dull, Green)
	getActionResult MadeChange = ("done", Vivid, Green)
	getActionResult FailedChange = ("failed", Vivid, Red)

{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Propellor.Types.ResultCheck (
	UncheckedProperty,
	unchecked,
	checkResult,
	Checkable,
	assume,
) where

import Propellor.Types
import Propellor.Exception

import Data.Monoid
import Control.Monad.IO.Class (liftIO)

-- | This is a `Property` but its `Result` is not accurate; in particular
-- it may return `NoChange` despite having made a change. However, when it
-- returns `MadeChange`, it really did made a change, and `FailedChange`
-- is still an error.
data UncheckedProperty i = UncheckedProperty (Property i)

-- | Use to indicate that a Property is unchecked.
unchecked :: Property i -> UncheckedProperty i
unchecked = UncheckedProperty

-- | Checks the result of a property. Mostly used to convert a
-- `UncheckedProperty` to a `Property`, but can also be used to further
-- check a `Property`.
checkResult 
	:: Checkable p i
	=> IO a
	-- ^ Run before ensuring the property.
	-> (a -> IO Result)
	-- ^ Run after ensuring the property. Return `MadeChange` if a
	-- change was detected, or `NoChange` if no change was detected.
	-> p i
	-> Property i
checkResult precheck postcheck p = adjustPropertySatisfy (checkedProp p) $ \satisfy -> do
	a <- liftIO precheck
	r <- catchPropellor satisfy
	-- Always run postcheck, even if the result is already MadeChange,
	-- as it may need to clean up after precheck.
	r' <- liftIO $ postcheck a
	return (r <> r')
	
class Checkable p i where
	checkedProp :: p i -> Property i

instance Checkable Property i where
	checkedProp = id

instance Checkable UncheckedProperty i where
	checkedProp (UncheckedProperty p) = p

-- | Sometimes it's not practical to test if a property made a change.
-- In such a case, it's often fine to say:
--
-- > someprop `assume` MadeChange
--
-- However, beware assuming `NoChange`, as that will make combinators
-- like `onChange` not work.
assume :: UncheckedProperty i -> Result -> Property i
assume (UncheckedProperty p) result = adjustPropertySatisfy (checkedProp p) $ \satisfy -> do
	r <- satisfy
	return (r <> result)

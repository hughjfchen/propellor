{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Propellor.Types.ResultCheck
  ( UncheckedProperty,
    PrevCheckResult (..),
    unchecked,
    checkResult,
    check,
    prevCheck,
    Checkable,
    assume,
  )
where

import Data.Monoid
import Data.Typeable (Typeable)
import Propellor.Exception
import Propellor.Message (actionMessageOn)
import Propellor.Types
import Utility.Monad
import Prelude

-- | The check result before carrying on the property action
-- NoNeedToCarryOn means the property had been fulfilled so
-- no need to carry on the property action
-- UnableToCarryOn means the property action is not be able
-- to be carried on due to some prerequisitves
-- CarryOn means the property action should be carried on to
-- fulfill the property
data PrevCheckResult
  = CarryOn
  | UnableToCarryOn String
  | NoNeedToCarryOn String
  deriving (Eq, Ord, Show, Typeable)

-- | This is a `Property` but its `Result` is not accurate; in particular
-- it may return `NoChange` despite having made a change.
--
-- However, when it returns `MadeChange`, it really did make a change,
-- and `FailedChange` is still an error.
data UncheckedProperty i = UncheckedProperty (Property i)

instance TightenTargets UncheckedProperty where
  tightenTargets (UncheckedProperty p) = UncheckedProperty (tightenTargets p)

-- | Use to indicate that a Property is unchecked.
unchecked :: Property i -> UncheckedProperty i
unchecked = UncheckedProperty

-- | Checks the result of a property. Mostly used to convert a
-- `UncheckedProperty` to a `Property`, but can also be used to further
-- check a `Property`.
checkResult ::
  (Checkable p i, LiftPropellor m) =>
  -- | Run before ensuring the property.
  m a ->
  -- | Run after ensuring the property. Return `MadeChange` if a
  -- change was detected, or `NoChange` if no change was detected.
  (a -> m Result) ->
  p i ->
  Property i
checkResult precheck postcheck p = adjustPropertySatisfy (checkedProp p) $ \satisfy -> do
  a <- liftPropellor precheck
  r <- catchPropellor satisfy
  -- Always run postcheck, even if the result is already MadeChange,
  -- as it may need to clean up after precheck.
  r' <- liftPropellor $ postcheck a
  return (r <> r')

-- | Makes a `Property` or an `UncheckedProperty` only run
-- when a check return value CarryOn.
prevCheck :: (Checkable p i, LiftPropellor m) => m PrevCheckResult -> p i -> Property i
prevCheck test p = adjustPropertySatisfy (preCheckedProp p) $ \satisfy -> do
  cResult <- liftPropellor test
  case cResult of
    CarryOn -> satisfy
    NoNeedToCarryOn noNeedMsg -> actionMessageOn "" noNeedMsg $ return NoChange
    UnableToCarryOn unableMsg -> actionMessageOn "" unableMsg $ return FailedChange

-- | Makes a `Property` or an `UncheckedProperty` only run
-- when a test succeeds.
check :: (Checkable p i, LiftPropellor m) => m Bool -> p i -> Property i
check test p = adjustPropertySatisfy (preCheckedProp p) $ \satisfy ->
  ifM
    (liftPropellor test)
    ( satisfy,
      return NoChange
    )

class Checkable p i where
  checkedProp :: p i -> Property i
  preCheckedProp :: p i -> Property i

instance Checkable Property i where
  checkedProp = id
  preCheckedProp = id

instance Checkable UncheckedProperty i where
  checkedProp (UncheckedProperty p) = p

  -- Since it was pre-checked that the property needed to be run,
  -- if the property succeeded, we can assume it made a change.
  preCheckedProp (UncheckedProperty p) = p `assume` MadeChange

-- | Sometimes it's not practical to test if a property made a change.
-- In such a case, it's often fine to say:
--
-- > someprop `assume` MadeChange
--
-- However, beware assuming `NoChange`, as that will make combinators
-- like `onChange` not work.
assume :: Checkable p i => p i -> Result -> Property i
assume p result = adjustPropertySatisfy (checkedProp p) $ \satisfy -> do
  r <- satisfy
  return (r <> result)

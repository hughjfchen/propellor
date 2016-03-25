{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Propellor.EnsureProperty
	( ensureProperty
	, property'
	, OuterMetaTypes
	) where

import Propellor.Types
import Propellor.Types.MetaTypes
import Propellor.Exception

-- | For when code running in the Propellor monad needs to ensure a
-- Property.
--
-- Use `property'` to get the `OuterMetaTypes`. For example:
--
-- > foo = Property Debian
-- > foo = property' $ \o -> do
-- > 	ensureProperty o (aptInstall "foo")
--
-- The type checker will prevent using ensureProperty with a property
-- that does not support the target OSes needed by the OuterMetaTypes.
-- In the example above, aptInstall must support Debian, since foo
-- is supposed to support Debian.
--
-- The type checker will also prevent using ensureProperty with a property
-- with HasInfo in its MetaTypes. Doing so would cause the `Info` associated
-- with the property to be lost.
ensureProperty
	::
		( (Targets inner `NotSuperset` Targets outer) ~ 'CanCombine
		, CannotUse_ensureProperty_WithInfo inner ~ 'True
		)
	=> OuterMetaTypes outer
	-> Property (MetaTypes inner)
	-> Propellor Result
ensureProperty _ = catchPropellor . getSatisfy

-- The name of this was chosen to make type errors a more understandable.
type family CannotUse_ensureProperty_WithInfo (l :: [a]) :: Bool
type instance CannotUse_ensureProperty_WithInfo '[] = 'True
type instance CannotUse_ensureProperty_WithInfo (t ': ts) =
	Not (t `EqT` 'WithInfo) && CannotUse_ensureProperty_WithInfo ts

-- | Constructs a property, like `property`, but provides its
-- `OuterMetaTypes`.
property'
	:: SingI metatypes
	=> Desc
	-> (OuterMetaTypes metatypes -> Propellor Result)
	-> Property (MetaTypes metatypes)
property' d a =
	let p = Property sing d (a (outerMetaTypes p)) mempty mempty
	in p

-- | Used to provide the metatypes of a Property to calls to 
-- 'ensureProperty` within it.
newtype OuterMetaTypes metatypes = OuterMetaTypes (MetaTypes metatypes)

outerMetaTypes :: Property (MetaTypes l) -> OuterMetaTypes l
outerMetaTypes (Property metatypes _ _ _ _) = OuterMetaTypes metatypes

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Propellor.Types.ConfigurableValue where

-- | A value that can be used in a configuration file, or otherwise used to
-- configure a program.
--
-- Unlike Show, there should only be instances of this type class for
-- values that have a standard serialization that is understood outside of
-- Haskell code.
--
-- When converting a type alias such as "type Foo = String" or "type Foo = Int"
-- to a newtype, it's unsafe to derive a Show instance, because there may
-- be code that shows the type to configure a value. Instead, define a
-- ConfigurableValue instance.
class ConfigurableValue t where
	val :: t -> String

instance ConfigurableValue String where
	val = id

instance ConfigurableValue Int where
	val = show

instance ConfigurableValue Integer where
	val = show

instance ConfigurableValue Float where
	val = show

instance ConfigurableValue Double where
	val = show

module Propellor.Types.Val where

import Data.Monoid

import Propellor.Types.Empty

data Val a = Val a | NoVal
	deriving (Eq, Show)

instance Monoid (Val a) where
	mempty = NoVal
	mappend old new = case new of
		NoVal -> old
		_ -> new

instance Empty (Val a) where
	isEmpty NoVal = True
	isEmpty _ = False

fromVal :: Val a -> Maybe a
fromVal (Val a) = Just a
fromVal NoVal = Nothing

{-# LANGUAGE DataKinds, PolyKinds, TypeOperators, TypeFamilies, GADTs #-}

module Propellor.Types.Singletons where

-- | The data family of singleton types.
data family Sing (x :: k)

-- | A class used to pass singleton values implicitly.
class SingI t where
	sing :: Sing t

-- Lists of singletons
data instance Sing (x :: [k]) where
	Nil :: Sing '[]
	Cons :: Sing x -> Sing xs -> Sing (x ': xs)
instance (SingI x, SingI xs) => SingI (x ': xs) where sing = Cons sing sing
instance SingI '[] where sing = Nil

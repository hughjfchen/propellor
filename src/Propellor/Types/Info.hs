{-# LANGUAGE DeriveDataTypeable #-}

module Propellor.Types.Info (
	Info,
	IsInfo(..),
	addInfo,
	getInfo,
	mapInfo,
	propigatableInfo,
	InfoVal(..),
	fromInfoVal,
	Typeable,
) where

import Data.Dynamic
import Data.Monoid
import Data.Maybe

-- | Information about a Host, which can be provided by its properties.
data Info = Info [(Dynamic, Bool)]

instance Show Info where
	show (Info l) = "Info " ++ show (map (dynTypeRep . fst) l)

instance Monoid Info where
 	mempty = Info []
	mappend (Info a) (Info b) = Info (a <> b)

-- | Values stored in Info must be members of this class.
--
-- This is used to avoid accidentially using other data types
-- as info, especially type aliases which coud easily lead to bugs.
-- We want a little bit of dynamic types here, but not too far..
class (Typeable v, Monoid v) => IsInfo v where
	-- | Should info of this type be propigated out of a
	-- container to its Host?
	propigateInfo :: v -> Bool

-- | Any value in the `IsInfo` type class can be added to an Info.
addInfo :: IsInfo v => Info -> v -> Info
addInfo (Info l) v = Info ((toDyn v, propigateInfo v):l)

getInfo :: IsInfo v => Info -> v
getInfo (Info l) = mconcat (mapMaybe (fromDynamic . fst) (reverse l))

-- | Maps a function over all values stored in the Info that are of the
-- appropriate type.
mapInfo :: IsInfo v => (v -> v) -> Info -> Info
mapInfo f (Info l) = Info (map go l)
  where
	go (i, p) = case fromDynamic i of
		Nothing -> (i, p)
		Just v -> (toDyn (f v), p)

-- | Filters out parts of the Info that should not propigate out of a
-- container.
propigatableInfo :: Info -> Info
propigatableInfo (Info l) = Info (filter snd l)

-- | Use this to put a value in Info that is not a monoid.
-- The last value set will be used. This info does not propigate
-- out of a container.
data InfoVal v = NoInfoVal | InfoVal v
	deriving (Typeable)

instance Monoid (InfoVal v) where
	mempty = NoInfoVal
	mappend _ v@(InfoVal _) = v
	mappend v NoInfoVal = v

instance Typeable v => IsInfo (InfoVal v) where
	propigateInfo _ = False

fromInfoVal :: InfoVal v -> Maybe v
fromInfoVal NoInfoVal = Nothing
fromInfoVal (InfoVal v) = Just v

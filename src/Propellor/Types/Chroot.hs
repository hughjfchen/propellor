module Propellor.Types.Chroot where

import Data.Monoid
import qualified Data.Map as M

data ChrootInfo h = ChrootInfo
	{ _chroots :: M.Map FilePath h
	}
	deriving (Show)

instance Monoid (ChrootInfo h) where
	mempty = ChrootInfo mempty
	mappend old new = ChrootInfo
		{ _chroots = M.union (_chroots old) (_chroots new)
		}

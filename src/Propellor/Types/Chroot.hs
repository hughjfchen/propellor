module Propellor.Types.Chroot where

import Data.Monoid
import qualified Data.Map as M

data ChrootInfo host = ChrootInfo
	{ _chroots :: M.Map FilePath host
	, _chrootCfg :: ChrootCfg
	}
	deriving (Show)

instance Monoid (ChrootInfo host) where
	mempty = ChrootInfo mempty mempty
	mappend old new = ChrootInfo
		{ _chroots = M.union (_chroots old) (_chroots new)
		, _chrootCfg = _chrootCfg old <> _chrootCfg new
		}

data ChrootCfg
	= NoChrootCfg
	| SystemdNspawnCfg [(String, Bool)]
	deriving (Show)

instance Monoid ChrootCfg where
	mempty = NoChrootCfg
	mappend v NoChrootCfg = v
	mappend NoChrootCfg v = v
	mappend (SystemdNspawnCfg l1) (SystemdNspawnCfg l2) =
		SystemdNspawnCfg (l1 <> l2)

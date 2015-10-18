{-# LANGUAGE DeriveDataTypeable #-}

module Propellor.Types.Chroot where

import Propellor.Types
import Propellor.Types.Empty
import Propellor.Types.Info

import Data.Monoid
import qualified Data.Map as M

data ChrootInfo = ChrootInfo
	{ _chroots :: M.Map FilePath Host
	, _chrootCfg :: ChrootCfg
	}
	deriving (Show, Typeable)

instance IsInfo ChrootInfo where
	propagateInfo _ = False

instance Monoid ChrootInfo where
	mempty = ChrootInfo mempty mempty
	mappend old new = ChrootInfo
		{ _chroots = M.union (_chroots old) (_chroots new)
		, _chrootCfg = _chrootCfg old <> _chrootCfg new
		}

instance Empty ChrootInfo where
	isEmpty i = and
		[ isEmpty (_chroots i)
		, isEmpty (_chrootCfg i)
		]

data ChrootCfg
	= NoChrootCfg
	| SystemdNspawnCfg [(String, Bool)]
	deriving (Show, Eq)

instance Monoid ChrootCfg where
	mempty = NoChrootCfg
	mappend v NoChrootCfg = v
	mappend NoChrootCfg v = v
	mappend (SystemdNspawnCfg l1) (SystemdNspawnCfg l2) =
		SystemdNspawnCfg (l1 <> l2)

instance Empty ChrootCfg where
	isEmpty c= c == NoChrootCfg

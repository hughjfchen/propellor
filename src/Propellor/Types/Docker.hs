{-# LANGUAGE DeriveDataTypeable #-}

module Propellor.Types.Docker where

import Propellor.Types
import Propellor.Types.Empty
import Propellor.Types.Info

import Data.Monoid
import qualified Data.Map as M

data DockerInfo = DockerInfo
	{ _dockerRunParams :: [DockerRunParam]
	, _dockerContainers :: M.Map String Host
	}
	deriving (Show, Typeable)

instance IsInfo DockerInfo where
	propagateInfo _ = False

instance Monoid DockerInfo where
	mempty = DockerInfo mempty mempty
	mappend old new = DockerInfo
		{ _dockerRunParams = _dockerRunParams old <> _dockerRunParams new
		, _dockerContainers = M.union (_dockerContainers old) (_dockerContainers new)
		}

instance Empty DockerInfo where
	isEmpty i = and
		[ isEmpty (_dockerRunParams i)
		, isEmpty (_dockerContainers i)
		]

newtype DockerRunParam = DockerRunParam (HostName -> String)

instance Show DockerRunParam where
	show (DockerRunParam a) = a ""

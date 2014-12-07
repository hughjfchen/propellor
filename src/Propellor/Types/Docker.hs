module Propellor.Types.Docker where

import Propellor.Types.OS
import Propellor.Types.Empty

import Data.Monoid
import qualified Data.Map as M

data DockerInfo h = DockerInfo
	{ _dockerRunParams :: [DockerRunParam]
	, _dockerContainers :: M.Map String h
	}
	deriving (Show)

instance Monoid (DockerInfo h) where
	mempty = DockerInfo mempty mempty
	mappend old new = DockerInfo
		{ _dockerRunParams = _dockerRunParams old <> _dockerRunParams new
		, _dockerContainers = M.union (_dockerContainers old) (_dockerContainers new)
		}

instance Empty (DockerInfo h) where
	isEmpty i = and
		[ isEmpty (_dockerRunParams i)
		, isEmpty (_dockerContainers i)
		]

newtype DockerRunParam = DockerRunParam (HostName -> String)

instance Show DockerRunParam where
	show (DockerRunParam a) = a ""

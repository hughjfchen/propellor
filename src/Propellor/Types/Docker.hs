module Propellor.Types.Docker where

import Propellor.Types.OS

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

newtype DockerRunParam = DockerRunParam (HostName -> String)

instance Show DockerRunParam where
	show (DockerRunParam a) = a ""

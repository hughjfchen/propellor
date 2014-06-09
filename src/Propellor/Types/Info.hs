module Propellor.Types.Info where

import Propellor.Types.OS
import qualified Propellor.Types.Dns as Dns

import qualified Data.Set as S
import Data.Monoid

-- | Information about a host.
data Info = Info
	{ _os :: Val System
	, _sshPubKey :: Val String
	, _dns :: S.Set Dns.Record
	, _namedconf :: Dns.NamedConfMap
	, _dockerinfo :: DockerInfo
	}
	deriving (Eq, Show)

instance Monoid Info where
	mempty = Info mempty mempty mempty mempty mempty
	mappend old new = Info
		{ _os = _os old <> _os new
		, _sshPubKey = _sshPubKey old <> _sshPubKey new
		, _dns = _dns old <> _dns new
		, _namedconf = _namedconf old <> _namedconf new
		, _dockerinfo = _dockerinfo old <> _dockerinfo new
		}

data Val a = Val a | NoVal
	deriving (Eq, Show)

instance Monoid (Val a) where
	mempty = NoVal
	mappend old new = case new of
		NoVal -> old
		_ -> new

fromVal :: Val a -> Maybe a
fromVal (Val a) = Just a
fromVal NoVal = Nothing

data DockerInfo = DockerInfo
	{ _dockerImage :: Val String
	, _dockerRunParams :: [HostName -> String]
	}

instance Eq DockerInfo where
	x == y = and
		[ _dockerImage x == _dockerImage y
		, let simpl v = map (\a -> a "") (_dockerRunParams v)
		  in simpl x == simpl y
		]

instance Monoid DockerInfo where
	mempty = DockerInfo mempty mempty
	mappend old new = DockerInfo
		{ _dockerImage = _dockerImage old <> _dockerImage new
		, _dockerRunParams = _dockerRunParams old <> _dockerRunParams new
		}

instance Show DockerInfo where
	show a = unlines
		[ "docker image " ++ show (_dockerImage a)
		, "docker run params " ++ show (map (\mk -> mk "") (_dockerRunParams a))
		]

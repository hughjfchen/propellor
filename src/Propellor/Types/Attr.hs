module Propellor.Types.Attr where

import Propellor.Types.OS
import qualified Propellor.Types.Dns as Dns

import qualified Data.Set as S
import Data.Monoid

-- | The attributes of a host.
data Attr = Attr
	{ _os :: Maybe System
	, _sshPubKey :: Maybe String
	, _dns :: S.Set Dns.Record
	, _namedconf :: Dns.NamedConfMap

	, _dockerImage :: Maybe String
	, _dockerRunParams :: [HostName -> String]
	}

instance Eq Attr where
	x == y = and
		[ _os x == _os y
		, _dns x == _dns y
		, _namedconf x == _namedconf y
		, _sshPubKey x == _sshPubKey y

		, _dockerImage x == _dockerImage y
		, let simpl v = map (\a -> a "") (_dockerRunParams v)
		  in simpl x == simpl y
		]

instance Monoid Attr where
	mempty = Attr Nothing Nothing mempty mempty Nothing mempty
	mappend old new = Attr
		{ _os = case _os new of
			Just v -> Just v
			Nothing -> _os old
		, _sshPubKey = case _sshPubKey new of
			Just v -> Just v
			Nothing -> _sshPubKey old
		, _dns = _dns new <> _dns old
		, _namedconf = _namedconf new <> _namedconf old
		, _dockerImage = case _dockerImage new of
			Just v -> Just v
			Nothing -> _dockerImage old
		, _dockerRunParams = _dockerRunParams old <> _dockerRunParams new
		}

instance Show Attr where
	show a = unlines
		[ "OS " ++ show (_os a)
		, "sshPubKey " ++ show (_sshPubKey a)
		, "dns " ++ show (_dns a)
		, "namedconf " ++ show (_namedconf a)
		, "docker image " ++ show (_dockerImage a)
		, "docker run params " ++ show (map (\mk -> mk "") (_dockerRunParams a))
		]

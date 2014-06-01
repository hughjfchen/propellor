module Propellor.Types.Attr where

import Propellor.Types.OS
import qualified Propellor.Types.Dns as Dns

import qualified Data.Set as S
import Data.Monoid

-- | The attributes of a host.
data Attr = Attr
	{ _os :: Val System
	, _sshPubKey :: Val String
	, _dns :: S.Set Dns.Record
	, _namedconf :: Dns.NamedConfMap

	, _dockerImage :: Val String
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
	mempty = Attr mempty mempty mempty mempty mempty mempty
	mappend old new = Attr
		{ _os = _os old <> _os new
		, _sshPubKey = _sshPubKey old <> _sshPubKey new
		, _dns = _dns new <> _dns old
		, _namedconf = _namedconf old <> _namedconf new
		, _dockerImage = _dockerImage old <> _dockerImage new
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

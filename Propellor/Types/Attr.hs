module Propellor.Types.Attr where

import Propellor.Types.OS
import qualified Propellor.Types.Dns as Dns

import qualified Data.Set as S

-- | The attributes of a host. For example, its hostname.
data Attr = Attr
	{ _hostname :: HostName
	, _os :: Maybe System
	, _dns :: S.Set Dns.Record
	, _sshPubKey :: Maybe String

	, _dockerImage :: Maybe String
	, _dockerRunParams :: [HostName -> String]
	}

instance Eq Attr where
	x == y = and
		[ _hostname x == _hostname y
		, _os x == _os y
		, _dns x == _dns y
		, _sshPubKey x == _sshPubKey y

		, _dockerImage x == _dockerImage y
		, let simpl v = map (\a -> a "") (_dockerRunParams v)
		  in simpl x == simpl y
		]

instance Show Attr where
	show a = unlines
		[ "hostname " ++ _hostname a
		, "OS " ++ show (_os a)
		, "dns " ++ show (_dns a)
		, "sshPubKey " ++ show (_sshPubKey a)
		, "docker image " ++ show (_dockerImage a)
		, "docker run params " ++ show (map (\mk -> mk "") (_dockerRunParams a))
		]

newAttr :: HostName -> Attr
newAttr hn = Attr hn Nothing S.empty Nothing Nothing []

type SetAttr = Attr -> Attr

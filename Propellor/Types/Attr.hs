module Propellor.Types.Attr where

import qualified Data.Set as S

-- | The attributes of a host. For example, its hostname.
data Attr = Attr
	{ _hostname :: HostName
	, _cnames :: S.Set Domain

	, _dockerImage :: Maybe String
	, _dockerRunParams :: [HostName -> String]
	}

instance Eq Attr where
	x == y = and
		[ _hostname x == _hostname y
		, _cnames x == _cnames y

		, _dockerImage x == _dockerImage y
		, let simpl v = map (\a -> a "") (_dockerRunParams v)
		  in simpl x == simpl y
		]

newAttr :: HostName -> Attr
newAttr hn = Attr hn S.empty Nothing []

type HostName = String
type Domain = String

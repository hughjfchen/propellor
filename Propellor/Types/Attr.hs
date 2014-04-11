module Propellor.Types.Attr where

import qualified Data.Set as S

-- | The attributes of a host. For example, its hostname.
data Attr = Attr
	{ _hostname :: HostName
	, _cnames :: S.Set Domain
	}
	deriving (Eq, Show)

newAttr :: HostName -> Attr
newAttr hn = Attr hn S.empty

type HostName = String
type Domain = String

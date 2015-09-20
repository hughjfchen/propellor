module Propellor.Property.Aiccu (
	installed,
	reloaded,
	confPath,
	UserName,
	TunnelId,
	hasConfig,
) where

import Propellor
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.Service as Service
import qualified Propellor.Property.File as File

installed :: Property NoInfo
installed = Apt.installed ["aiccu"]

reloaded :: Property NoInfo
reloaded = Service.reloaded "aiccu"

confPath :: FilePath
confPath = "/etc/aiccu.conf"

type TunnelId = String

config :: UserName -> TunnelId -> PrivData -> [File.Line]
config u t p = 
	[ "protocol tic"
	, "server tic.sixxs.net"
	, "username " ++ u
	, "password " ++ (privDataVal p)
	, "ipv6_interface sixxs"
	, "tunnel_id " ++ t
	, "daemonize true"
	, "automatic true"
	, "requiretls true"
	, "makebeats true"
	]

-- | Configures an ipv6 tunnel using sixxs.net, with the given TunneId
-- and sixx.net UserName.
hasConfig :: TunnelId -> UserName -> Property HasInfo
hasConfig t u = prop  `onChange` reloaded
  where
	prop = withSomePrivData [(Password (u++"/"++t)), (Password u)] (Context "aiccu") $
		property "aiccu configured" . writeConfig
	writeConfig :: (((PrivDataField, PrivData) -> Propellor Result) -> Propellor Result) -> Propellor Result
	writeConfig getpassword = getpassword $ ensureProperty . go
	go (Password _, p) = confPath `File.hasContent` config u t p
	go (f, _) = error $ "Unexpected type of privdata: " ++ show f

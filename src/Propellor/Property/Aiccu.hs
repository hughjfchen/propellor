module Propellor.Property.Aiccu where

import Propellor
import qualified Propellor.Property.Apt as Apt

confPath :: FilePath
confPath = "/etc/aiccu.conf"

config :: String -> String -> PrivData -> [String]
config u t p = [ "protocol tic"
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

hasConfig :: String -> String -> Property HasInfo
hasConfig t u = withSomePrivData [(Password (u++"/"++t)), (Password u)] (Context "aiccu") $ property "aiccu configured" . writeConfig
	where writeConfig :: (((PrivDataField, PrivData) -> Propellor Result) -> Propellor Result) -> Propellor Result
	      writeConfig getpassword = getpassword $ go
	      go (Password u, p) = makeChange $ writeFile confPath (unlines $ config u t p)
	      go (f, _) = error $ "Unexpected type of privdata: " ++ show f

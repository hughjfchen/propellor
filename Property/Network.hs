module Property.Network where

import Common
import qualified Property.File as File

interfaces :: FilePath
interfaces = "/etc/network/interfaces"

-- 6to4 ipv6 connection, should work anywhere
ipv6to4 :: Property
ipv6to4 = fileProperty "ipv6to4" go interfaces
	`onChange` ifUp "sit0"
  where
	go ls
		| all (`elem` ls) stanza = ls
		| otherwise = ls ++ stanza
	stanza =
		[ "# Automatically added by propeller"
		, "iface sit0 inet6 static"
		, "	address 2002:5044:5531::1"
		, "	netmask 64"
		, "	gateway ::192.88.99.1"
		, "# End automatically added by propeller"

ifUp :: String -> Property
ifUp iface = cmdProperty "ifup" [Param iface]

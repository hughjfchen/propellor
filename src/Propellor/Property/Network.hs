module Propellor.Property.Network where

import Propellor
import Propellor.Property.File

interfaces :: FilePath
interfaces = "/etc/network/interfaces"

-- | 6to4 ipv6 connection, should work anywhere
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
		, "\taddress 2002:5044:5531::1"
		, "\tnetmask 64"
		, "\tgateway ::192.88.99.1"
		, "auto sit0"
		, "# End automatically added by propeller"
		]

type Interface = String

ifUp :: Interface -> Property
ifUp iface = cmdProperty "ifup" [iface]

module Propellor.Property.Network where

import Propellor
import Propellor.Property.File

type Interface = String

interfaces :: FilePath
interfaces = "/etc/network/interfaces"

interfaceFile :: Interface -> FilePath
interfaceFile iface = "/etc/network/interfaces.d" </> iface

-- | Enable source-directory interfaces.d
interfacesD :: Property
interfacesD = containsLine interfaces "source-directory interfaces.d"
	`describe` "interfaces.d directory enabled"

-- | 6to4 ipv6 connection, should work anywhere
ipv6to4 :: Property
ipv6to4 = hasContent (interfaceFile "sit0")
	[ "# Automatically added by propeller"
	, "iface sit0 inet6 static"
	, "\taddress 2002:5044:5531::1"
	, "\tnetmask 64"
	, "\tgateway ::192.88.99.1"
	, "auto sit0"
	]
	`describe` "ipv6to4"
	`requires` interfacesD
	`onChange` ifUp "sit0"

ifUp :: Interface -> Property
ifUp iface = cmdProperty "ifup" [iface]

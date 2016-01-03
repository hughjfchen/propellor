module Propellor.Property.HostingProvider.CloudAtCost where

import Propellor.Base
import qualified Propellor.Property.Hostname as Hostname
import qualified Propellor.Property.File as File
import qualified Propellor.Property.User as User

-- Clean up a system as installed by cloudatcost.com
decruft :: Property NoInfo
decruft = propertyList "cloudatcost cleanup"
	[ Hostname.sane
	, "worked around grub/lvm boot bug #743126" ==>
		"/etc/default/grub" `File.containsLine` "GRUB_DISABLE_LINUX_UUID=true"
		`onChange` (cmdProperty "update-grub" [] `assume` MadeChange)
		`onChange` (cmdProperty "update-initramfs" ["-u"] `assume` MadeChange)
	, combineProperties "nuked cloudatcost cruft"
		[ File.notPresent "/etc/rc.local"
		, File.notPresent "/etc/init.d/S97-setup.sh"
		, File.notPresent "/zang-debian.sh"
		, File.notPresent "/bin/npasswd"
		, User.nuked (User "user") User.YesReallyDeleteHome
		]
	]


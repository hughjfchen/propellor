module Propellor.Property.HostingProvider.CloudAtCost where

import Propellor
import qualified Propellor.Property.Hostname as Hostname
import qualified Propellor.Property.File as File
import qualified Propellor.Property.Ssh as Ssh
import qualified Propellor.Property.User as User

-- Clean up a system as installed by cloudatcost.com
decruft :: Property
decruft = propertyList "cloudatcost cleanup"
	[ Hostname.sane
	, Ssh.randomHostKeys
	, "worked around grub/lvm boot bug #743126" ==>
		"/etc/default/grub" `File.containsLine` "GRUB_DISABLE_LINUX_UUID=true"
		`onChange` cmdProperty "update-grub" []
		`onChange` cmdProperty "update-initramfs" ["-u"]
	, combineProperties "nuked cloudatcost cruft"
		[ File.notPresent "/etc/rc.local"
		, File.notPresent "/etc/init.d/S97-setup.sh"
		, User.nuked "user" User.YesReallyDeleteHome
		]
	]


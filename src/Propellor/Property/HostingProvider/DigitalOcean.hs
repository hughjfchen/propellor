module Propellor.Property.HostingProvider.DigitalOcean where

import Propellor
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.File as File

-- Digital Ocean does not provide any way to boot
-- the kernel provided by the distribution, except using kexec.
-- Without this, some old, and perhaps insecure kernel will be used.
--
-- Note that this only causes the new kernel to be loaded on reboot.
-- If the power is cycled, the old kernel still boots up.
-- TODO: detect this and reboot immediately?
distroKernel :: Property
distroKernel = propertyList "digital ocean distro kernel hack"
	[ Apt.installed ["grub-pc", "kexec-tools"]
	, "/etc/default/kexec" `File.containsLines`
		[ "LOAD_KEXEC=true"
		, "USE_GRUB_CONFIG=true"
		] `describe` "kexec configured"
	]

-- | Maintainer: Sean Whitton <spwhitton@spwhitton.name>

module Propellor.Property.HostingProvider.Exoscale (
	distroKernel,
) where

import Propellor.Base
import qualified Propellor.Property.File as File
import qualified Propellor.Property.Grub as Grub
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.Reboot as Reboot

-- | The current Exoshare Debian image doesn't install GRUB, so this property
-- makes sure GRUB is installed and correctly configured
--
-- In case an old, insecure kernel is running, we check for an old kernel
-- version and reboot immediately if one is found.
--
-- Note that we ignore anything after the first hyphen when considering whether
-- the running kernel's version is older than the Debian-supplied kernel's
-- version.
distroKernel :: Architecture -> Property DebianLike
distroKernel arch = go `flagFile` theFlagFile
  where
	go = combineProperties "boots distro kernel" $ props
		& Apt.installed ["grub2", "linux-image-" ++ arch]
		& Grub.boots "/dev/vda"
		& Grub.mkConfig
		-- Since we're rebooting we have to manually create the flagfile
		& File.hasContent theFlagFile [""]
		& Reboot.toDistroKernel
	theFlagFile = "/etc/propellor-distro-kernel"

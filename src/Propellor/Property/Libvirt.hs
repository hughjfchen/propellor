-- | Maintainer: Sean Whitton <spwhitton@spwhitton.name>

module Propellor.Property.Libvirt (
	NumVCPUs,
	MiBMemory,
	AutoStart(..),
	DiskImageType(..),
	installed,
	defaultNetworkAutostarted,
	kvmDefined,
) where

import Propellor.Base
import Propellor.Types.Info
import Propellor.Property.Chroot
import Propellor.Property.DiskImage
import Propellor.Property.Chroot.Util (removeChroot)
import qualified Propellor.Property.Apt as Apt

-- | The number of virtual CPUs to assign to the virtual machine
type NumVCPUs = Int

-- | The number of MiB of memory to assign to the virtual machine
type MiBMemory = Int

-- | Whether the virtual machine should be started after it is defined, and at
-- host system boot
data AutoStart = AutoStart | NoAutoStart

-- | Which type of disk image to build for the virtual machine
data DiskImageType = Raw | QCow2

-- | Install basic libvirt components
installed :: Property DebianLike
installed = Apt.installed ["libvirt-clients", "virtinst"]

-- | Ensure that the default libvirt network is set to autostart.
--
-- On Debian, it is not started by default after installation of libvirt.
defaultNetworkAutostarted :: Property DebianLike
defaultNetworkAutostarted = check (not <$> doesFileExist autostartFile)
	(cmdProperty "virsh" ["net-autostart", "default"])
	`requires` installed
  where
	autostartFile = "/etc/libvirt/qemu/networks/autostart/default.xml"

-- | Builds a disk image with the properties of the given Host, installs a
-- libvirt configuration file to boot the image, and if it is set to autostart,
-- start the VM.
--
-- Note that building the disk image happens only once.  So if you change the
-- properties of the given Host, this property will not modify the disk image.
-- In order to later apply properties to the VM, you should spin it directly, or
-- arrange to have it spun with a property like 'Cron.runPropellor', or use
-- 'Propellor.Property.Conductor' from the VM host.
--
-- Suggested usage in @config.hs@:
--
-- > mybox = host "mybox.example.com" $ props
-- > 	& osDebian (Stable "stretch") X86_64
-- >    & Libvirt.defaultNetworkAutostarted
-- > 	`onChange` (cmdProperty "virsh" ["net-start", "default"]
-- > 		`assume` MadeChange)
-- > 	& Libvirt.kvmDefined Libvirt.Raw 2048 2 Libvirt.NoAutoStart subbox
-- >
-- > subbox = host "subbox.mybox.example.com" $ props
-- > 	& osDebian Unstable X86_64
-- > 	& hasPartition
-- > 		( partition EXT4
-- > 			`mountedAt` "/"
-- > 			`addFreeSpace` MegaBytes 10240
-- > 		)
-- > 	& Apt.installed ["linux-image-amd64"]
-- > 	& Grub.installed PC
-- >
-- > 	& ipv4 "192.168.122.31"
-- > 	& Network.static "ens3" (IPv4 "192.168.122.31")
-- > 		(Just (Network.Gateway (IPv4 "192.168.122.1")))
-- > 		`requires` Network.cleanInterfacesFile
-- > 	& Hostname.sane
kvmDefined
	:: DiskImageType
	-> MiBMemory
	-> NumVCPUs
	-> AutoStart
	-> Host
	-> Property (HasInfo + DebianLike)
kvmDefined imageType mem cpus auto h =
	(built `before` nuked `before` defined `before` started)
	`requires` installed
  where
	built :: Property (HasInfo + DebianLike)
	built = check (not <$> doesFileExist imageLoc)
		(setupRevertableProperty $ imageBuiltFor h
			(image) (Debootstrapped mempty))
	nuked :: Property UnixLike
	nuked = check (doesDirectoryExist (imageLoc <.> "chroot"))
		(property "destroy the chroot used to build the image" $ do
			liftIO $ removeChroot (imageLoc <.> "chroot")
			liftIO $ nukeFile (imageLoc <.> "parttable")
			return MadeChange)
	defined :: Property UnixLike
	defined = check (not <$> doesFileExist conf)
		(scriptProperty
			[ "virt-install -n " ++ hostName h
				++ osTypeArg ++ osVariantArg
				++ " --memory=" ++ show mem
				++ " --vcpus=" ++ show cpus
				++ " --disk path=" ++ imageLoc
					++ ",device=disk,bus=virtio"
				++ autoStartArg
				++ " --print-xml"
				++ " >" ++ confTmp
			, "virsh define " ++ confTmp
			, "rm " ++ confTmp
			])
	started :: Property UnixLike
	started = case AutoStart of
		AutoStart -> cmdProperty "virsh" ["start", hostName h]
			`assume` MadeChange
		NoAutoStart -> doNothing

	image = case imageType of
		Raw -> RawDiskImage imageLoc
	imageLoc =
		"/var/lib/libvirt/images" </> hostName h <.> case imageType of
			Raw -> "img"
	conf = "/etc/libvirt/qemu" </> hostName h <.> "xml"
	confTmp = conf <.> "tmp"

	osTypeArg = maybe "" (" --os-type=" ++) $ osType h
	osVariantArg = maybe "" (" --os-variant=" ++) $ osVariant h
	autoStartArg = case auto of
		AutoStart -> " --autostart"
		NoAutoStart -> ""

-- ==== utility functions ====

osType :: Host -> Maybe String
osType h = hostSystem h >>= \s -> case s of
	System (Debian Linux _) _ -> Just "Linux"
	System (Buntish _) _      -> Just "Linux"
	System ArchLinux _        -> Just "Linux"
	_                         -> Nothing

-- TODO specify more of these
osVariant :: Host -> Maybe String
osVariant h = hostSystem h >>= \s -> case s of
	System (Debian _ (Stable "stretch")) _ -> Just "debian9"
	_                                      -> Nothing

hostSystem :: Host -> Maybe System
hostSystem = fromInfoVal . fromInfo . hostInfo

-- | Maintainer: Sean Whitton <spwhitton@spwhitton.name>

module Propellor.Property.Libvirt (
	NumVCPUs(..),
	MiBMemory(..),
	AutoStart(..),
	DiskImageType(..),
	installed,
	defaultNetworkAutostarted,
	defined,
) where

import Propellor.Base
import Propellor.Types.Info
import Propellor.Property.Chroot
import Propellor.Property.DiskImage
import Propellor.Property.Chroot.Util (removeChroot)
import qualified Propellor.Property.Apt as Apt

-- | The number of virtual CPUs to assign to the virtual machine
newtype NumVCPUs = NumVCPUs Int

-- | The number of MiB of memory to assign to the virtual machine
newtype MiBMemory = MiBMemory Int

-- | Whether the virtual machine should be started after it is defined, and at
-- host system boot
data AutoStart = AutoStart | NoAutoStart

-- | Which type of disk image to build for the virtual machine
data DiskImageType = Raw -- | QCow2

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
-- > 	& Libvirt.defined Libvirt.Raw
-- >		(Libvirt.MiBMemory 2048) (Libvirt.NumVCPUs 2)
-- >		Libvirt.NoAutoStart subbox
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
defined
	:: DiskImageType
	-> MiBMemory
	-> NumVCPUs
	-> AutoStart
	-> Host
	-> Property (HasInfo + DebianLike)
defined imageType (MiBMemory mem) (NumVCPUs cpus) auto h =
	(built `before` nuked `before` xmlDefined `before` started)
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
	xmlDefined :: Property UnixLike
	xmlDefined = check (not <$> doesFileExist conf)
		(scriptProperty
			[ "virt-install -n " ++ hostName h
				++ osVariantArg
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
	started = case auto of
		AutoStart -> scriptProperty
			[ "virsh list | grep -q \""
				++ hostName h ++ " .*running\" && exit 0"
			, "virsh start " ++ hostName h
			] `assume` NoChange
		NoAutoStart -> doNothing

	image = case imageType of
		Raw -> RawDiskImage imageLoc
	imageLoc =
		"/var/lib/libvirt/images" </> hostName h <.> case imageType of
			Raw -> "img"
	conf = "/etc/libvirt/qemu" </> hostName h <.> "xml"
	confTmp = conf <.> "tmp"

	osVariantArg = maybe "" (" --os-variant=" ++) $ osVariant h
	autoStartArg = case auto of
		AutoStart -> " --autostart"
		NoAutoStart -> ""

-- ==== utility functions ====

osVariant :: Host -> Maybe String
osVariant h = hostSystem h >>= \s -> case s of
	System (Debian _ (Stable "jessie")) _ -> Just "debian8"
	System (Debian _ (Stable "stretch")) _ -> Just "debian9"
	System (Debian _ Testing) _ -> Just "debiantesting"
	System (Debian _ Unstable) _ -> Just "debiantesting"

	System (Buntish "trusty") _ -> Just "ubuntu14.04"
	System (Buntish "utopic") _ -> Just "ubuntu14.10"
	System (Buntish "vivid") _ -> Just "ubuntu15.04"
	System (Buntish "wily") _ -> Just "ubuntu15.10"
	System (Buntish "xenial") _ -> Just "ubuntu16.04"
	System (Buntish "yakkety") _ -> Just "ubuntu16.10"
	System (Buntish "zesty") _ -> Just "ubuntu17.04"
	System (Buntish "artful") _ -> Just "ubuntu17.10"
	System (Buntish "bionic") _ -> Just "ubuntu18.04"

	System (FreeBSD (FBSDProduction FBSD101)) _ -> Just "freebsd10.1"
	System (FreeBSD (FBSDProduction FBSD102)) _ -> Just "freebsd10.2"
	System (FreeBSD (FBSDProduction FBSD093)) _ -> Just "freebsd9.3"
	System (FreeBSD (FBSDLegacy FBSD101)) _ -> Just "freebsd10.1"
	System (FreeBSD (FBSDLegacy FBSD102)) _ -> Just "freebsd10.2"
	System (FreeBSD (FBSDLegacy FBSD093)) _ -> Just "freebsd9.3"

	-- libvirt doesn't have an archlinux variant yet, it seems
	System ArchLinux _ -> Nothing

	-- other stable releases that we don't know about (since there are
	-- infinitely many possible stable release names, as it is a freeform
	-- string, we need this to avoid a compiler warning)
	System (Debian _ _) _ -> Nothing
	System (Buntish _) _ -> Nothing

hostSystem :: Host -> Maybe System
hostSystem = fromInfoVal . fromInfo . hostInfo

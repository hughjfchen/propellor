-- | Maintainer: Sean Whitton <spwhitton@spwhitton.name>

module Propellor.Property.Libvirt (
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

type NumVCPUs = Int
type MiBMemory = Int
data AutoStart = AutoStart | NoAutoStart
data DiskImageType = Raw | QCow2

installed :: Property DebianLike
installed = Apt.installed ["libvirt-clients", "virtinst"]

defaultNetworkAutostarted :: Property UnixLike
defaultNetworkAutostarted = check (not <$> doesFileExist autostartFile)
	(cmdProperty "virsh" ["net-autostart", "default"])
  where
	autostartFile = "/etc/libvirt/qemu/networks/autostart/default.xml"

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
	nuked = property "destroy the chroot used to build the image" $ do
			liftIO $ removeChroot (imageLoc <.> "chroot")
			liftIO $ nukeFile (imageLoc <.> "parttable")
			return MadeChange
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
			Raw -> "xml"
	conf = "/etc/libvirt/qemu" </> hostName h <.> "xml"
	confTmp = conf <.> "tmp"

	osTypeArg = maybe "" ("--os-type=" ++) $ osType h
	osVariantArg = maybe "" ("--os-variant=" ++) $ osVariant h
	autoStartArg = case auto of
		AutoStart -> " --autostart"
		NoAutoStart -> ""

osType :: Host -> Maybe String
osType h = hostSystem h >>= \s -> case s of
	System (Debian Linux _) _ -> Just "Linux"
	System (Buntish _) _      -> Just "Linux"
	System ArchLinux _        -> Just "Linux"
	_                         -> Nothing

osVariant :: Host -> Maybe String
osVariant h = hostSystem h >>= \s -> case s of
	System (Debian _ (Stable "stretch")) _ -> Just "debian9"
	_                                      -> Nothing

hostSystem :: Host -> Maybe System
hostSystem = fromInfoVal . fromInfo . hostInfo

module Propellor.Property.Grub where

import Propellor
import qualified Propellor.Property.File as File
import qualified Propellor.Property.Apt as Apt

-- | Eg, hd0,0 or xen/xvda1
type GrubDevice = String

type TimeoutSecs = Int

-- | Use PV-grub chaining to boot
--
-- Useful when the VPS's pv-grub is too old to boot a modern kernel image.
--
-- http://notes.pault.ag/linode-pv-grub-chainning/
--
-- The rootdev should be in the form "hd0", while the bootdev is in the form
-- "xen/xvda".
chainPVGrub :: GrubDevice -> GrubDevice -> TimeoutSecs -> Property
chainPVGrub rootdev bootdev timeout = combineProperties desc
	[ File.dirExists "/boot/grub"
	, "/boot/grub/menu.lst" `File.hasContent`
		[ "default 1" 
		, "timeout " ++ show timeout
		, ""
		, "title grub-xen shim"
		, "root (" ++ rootdev ++ ")"
		, "kernel /boot/xen-shim"
		, "boot"
		]
	, "/boot/load.cf" `File.hasContent`
		[ "configfile (" ++ bootdev ++ ")/boot/grub/grub.cfg" ]
	, Apt.installed ["grub-xen"]
	, flagFile (scriptProperty ["update-grub; grub-mkimage --prefix '(" ++ bootdev ++ ")/boot/grub' -c /boot/load.cf -O x86_64-xen /usr/lib/grub/x86_64-xen/*.mod > /boot/xen-shim"]) "/boot/xen-shim"
			`describe` "/boot-xen-shim"
	]
  where
	desc = "chain PV-grub"

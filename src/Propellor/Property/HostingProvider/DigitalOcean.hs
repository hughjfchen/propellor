module Propellor.Property.HostingProvider.DigitalOcean (
	distroKernel
) where

import Propellor
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.File as File
import qualified Propellor.Property.Reboot as Reboot

import Data.List

-- | Digital Ocean does not provide any way to boot
-- the kernel provided by the distribution, except using kexec.
-- Without this, some old, and perhaps insecure kernel will be used.
--
-- This property causes the distro kernel to be loaded on reboot, using kexec.
--
-- If the power is cycled, the non-distro kernel still boots up.
-- So, this property also checks if the running kernel is present in /boot,
-- and if not, reboots immediately into a distro kernel.
distroKernel :: Property NoInfo
distroKernel = propertyList "digital ocean distro kernel hack"
	[ Apt.installed ["grub-pc", "kexec-tools", "file"]
	, "/etc/default/kexec" `File.containsLines`
		[ "LOAD_KEXEC=true"
		, "USE_GRUB_CONFIG=true"
		] `describe` "kexec configured"
	, check (not <$> runningInstalledKernel) Reboot.now
		`describe` "running installed kernel"
	]

runningInstalledKernel :: IO Bool
runningInstalledKernel = do
	kernelver <- takeWhile (/= '\n') <$> readProcess "uname" ["-r"]
	when (null kernelver) $
		error "failed to read uname -r"
	kernelimages <- concat <$> mapM kernelsIn ["/", "/boot/"] 
	when (null kernelimages) $
		error "failed to find any installed kernel images"
	findVersion kernelver <$>
		readProcess "file" ("-L" : kernelimages)

-- | File output looks something like this, we want to unambiguously
-- match the running kernel version:
--   Linux kernel x86 boot executable bzImage, version 3.16-3-amd64 (debian-kernel@lists.debian.org) #1 SMP Debian 3.1, RO-rootFS, swap_dev 0x2, Normal VGA
findVersion :: String -> String -> Bool
findVersion ver s = (" version " ++ ver ++ " ") `isInfixOf` s

kernelsIn :: FilePath -> IO [FilePath]
kernelsIn d = filter ("vmlinu" `isInfixOf`) <$> dirContents d

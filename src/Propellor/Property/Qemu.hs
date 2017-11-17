module Propellor.Property.Qemu where

import Propellor.Base
import qualified Propellor.Property.Apt as Apt

-- | Installs qemu user mode emulation binaries, built statically,
-- which allow foreign binaries to run directly.
foreignBinariesEmulated :: RevertableProperty Linux Linux
foreignBinariesEmulated = (setup <!> cleanup)
	`describe` "foreign binary emulation"
  where
	setup = Apt.installed p `pickOS` unsupportedOS
	cleanup = Apt.removed p `pickOS` unsupportedOS
	p = ["qemu-user-static"]

-- | Removes qemu user mode emulation binary for the host CPU.
-- This binary is copied into a chroot by qemu-debootstrap, and is not
-- part of any package.
--
-- Note that removing the binary will prevent using the chroot on the host
-- system.
--
-- The FilePath is the path to the top of the chroot.
removeHostEmulationBinary :: FilePath -> Property Linux
removeHostEmulationBinary top = tightenTargets $ 
	scriptProperty ["rm -f " ++ top ++ "/usr/bin/qemu-*-static"]
		`assume` MadeChange

-- | Check if the given System supports an Architecture.
--
-- For example, on Debian, X86_64 supports X86_32, and vice-versa.
supportsArch :: System -> Architecture -> Bool
supportsArch (System os a) b
	| a == b = True
	| otherwise = case os of
		Debian _ _ -> debianlike
		Buntish _ -> debianlike
		-- don't know about other OS's
		_ -> False
  where
	debianlike =
		let l = 
			[ (X86_64, X86_32)
			, (ARMHF, ARMEL)
			, (PPC, PPC64)
			, (SPARC, SPARC64)
			, (S390, S390X)
			]
		in elem (a, b) l || elem (b, a) l

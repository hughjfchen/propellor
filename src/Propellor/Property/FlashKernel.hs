-- | Make ARM systems bootable using Debian's flash-kernel package.

module Propellor.Property.FlashKernel where

import Propellor.Base
import qualified Propellor.Property.File as File
import qualified Propellor.Property.Apt as Apt
import Propellor.Types.Bootloader
import Propellor.Types.Info

-- | A machine name, such as "Cubietech Cubietruck" or "Olimex A10-OLinuXino-LIME"
--
-- flash-kernel supports many different machines,
-- see its file /usr/share/flash-kernel/db/all.db for a list.
type Machine = String

-- | Uses flash-kernel to make a machine bootable.
--
-- Before using this, an appropriate kernel needs to already be installed, 
-- and on many machines, u-boot needs to be installed too.
installed :: Machine -> Property (HasInfo + DebianLike)
installed machine = setInfoProperty go (toInfo [FlashKernelInstalled])
  where
	go = "/etc/flash-kernel/machine" `File.hasContent` [machine]
		`onChange` (cmdProperty "flash-kernel" [] `assume` MadeChange)
		`requires` File.dirExists "/etc/flash-kernel"
		`requires` Apt.installed ["flash-kernel"]

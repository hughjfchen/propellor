-- | Machine-specific properties.
--
-- Many embedded computers have their own special configuration needed
-- to use them. Rather than needing to hunt down documentation about the
-- kernel, bootloader, etc needed by a given machine, if there's a property
-- in here for your machine, you can simply use it.
--
-- You will need to configure the `Host` with the right `Architecture`
-- for the machines. These properties do test at runtime that a supported
-- Architecture was selected.
--
-- Sometimes non-free firmware is needed to use a board. If the board won't
-- be functional at all without it, its property will include the non-free
-- firmware, but if the non-free firmware is only needed for non-critical
-- functionality, it won't be included.

module Propellor.Property.Machine (
	-- * ARM boards
	Marvell_SheevaPlug_BootDevice(..),
	marvell_SheevaPlug,
	cubietech_Cubietruck,
	olimex_A10_OLinuXino_LIME
) where

import Propellor.Base
import Propellor.Types.Core
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.FlashKernel as FlashKernel
import qualified Propellor.Property.Uboot as Uboot

data Marvell_SheevaPlug_BootDevice
	= Marvell_SheevaPlug_SDCard
	| Marvell_SheevaPlug_ESATA

-- | Marvel SheevaPlug
--
-- Note that u-boot may need to be upgraded manually, and will need to be
-- configured to boot from the SD card or eSATA. See
-- https://www.cyrius.com/debian/kirkwood/sheevaplug/install/
marvell_SheevaPlug :: Marvell_SheevaPlug_BootDevice -> Property (HasInfo + DebianLike)
marvell_SheevaPlug Marvell_SheevaPlug_SDCard =
	FlashKernel.installed "Marvell SheevaPlug Reference Board"
		`requires` kirkwood
marvell_SheevaPlug Marvell_SheevaPlug_ESATA =
	FlashKernel.installed "Marvell eSATA SheevaPlug Reference Board"
		`requires` kirkwood

-- | Cubietech Cubietruck (untested)
-- 
-- Wifi needs non-free firmware-brcm80211, whicn is not installed by
-- this property. Also, see https://bugs.debian.org/844056
cubietech_Cubietruck :: Property (HasInfo + DebianLike)
cubietech_Cubietruck = FlashKernel.installed "Cubietech Cubietruck"
		`requires` sunixi "Cubietruck"
		`requires` lpae

-- | Olimex A10-OLinuXino-LIME (untested)
olimex_A10_OLinuXino_LIME :: Property (HasInfo + DebianLike)
olimex_A10_OLinuXino_LIME = FlashKernel.installed "Olimex A10-OLinuXino-LIME"
		`requires` sunixi "A10-OLinuXino-Lime"
		`requires` armmp

sunixi :: Uboot.BoardName -> Property (HasInfo + DebianLike)
sunixi boardname = Uboot.sunxi boardname
	`requires` Apt.installed
		[ "firmware-linux-free"
		, "sunxi-tools"
		]

armmp :: Property DebianLike
armmp = checkArchitecture [ARMHF, ARMEL] $
	Apt.installed ["linux-image-armmp"]

lpae :: Property DebianLike
lpae = checkArchitecture [ARMHF, ARMEL] $ 
	Apt.installed ["linux-image-armmp-lpae"]

kirkwood :: Property DebianLike
kirkwood = checkArchitecture [ARMEL] $
	Apt.installed ["linux-image-kirkwwood"]

checkArchitecture :: [Architecture] -> Property DebianLike -> Property DebianLike
checkArchitecture as p = withOS (getDesc p) $ \w o -> case o of
	(Just (System _ arch)) | arch `elem` as -> ensureProperty w p
	_ -> unsupportedOS' -- error $ "Machine needs architecture to be one of: " ++ show as

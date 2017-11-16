-- | Machine-specific properties.
--
-- Many embedded computers have their own special configuration needed
-- to use them. Rather than needing to hunt down documentation about the
-- kernel, bootloader, etc needed by a given board, if there's a property
-- in here for your board, you can simply use it.

module Propellor.Property.Machine (
	-- * ARM boards
	Olimex_A10_OLinuXino_LIME,
	Cubietech Cubietruck
)

-- | Olimex A10-OLinuXino-LIME
Olimex_A10_OLinuXino_LIME :: Property (HasInfo + DebianLike)
Olimex_A10_OLinuXino_LIME = FlashKernel.installed "Olimex A10-OLinuXino-LIME"
	`requires` sunixi
	`requires` armmp

-- | Cubietech Cubietruck
Cubietech_Cubietruck :: Property (HasInfo + DebianLike)
Cubietech_Cubietruck = FlashKernel.installed "Cubietech Cubietruck"
	`requires` sunixi
	`requires` lpae

sunixi :: Property DebianLike
sunixi = Apt.installed
	[ "firmware-linux-free"
	, "u-boot"
	, "sunxi-tools"
	]

armmp :: Property DebianLike
armmp = Apt.installed ["linux-image-armmp"]

lpae :: Property DebianLike
lpae = Apt.installed ["linux-image-armmp-lpae"]

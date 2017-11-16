-- | Machine-specific properties.
--
-- Many embedded computers have their own special configuration needed
-- to use them. Rather than needing to hunt down documentation about the
-- kernel, bootloader, etc needed by a given machine, if there's a property
-- in here for your machine, you can simply use it.

module Propellor.Property.Machine (
	-- * ARM boards
	Cubietech Cubietruck,
	Olimex_A10_OLinuXino_LIME
)

-- | Cubietech Cubietruck
Cubietech_Cubietruck :: Property (HasInfo + DebianLike)
Cubietech_Cubietruck = FlashKernel.installed "Cubietech Cubietruck"
	`requires` sunixi
	`requires` lpae

-- | Olimex A10-OLinuXino-LIME
Olimex_A10_OLinuXino_LIME :: Property (HasInfo + DebianLike)
Olimex_A10_OLinuXino_LIME = FlashKernel.installed "Olimex A10-OLinuXino-LIME"
	`requires` sunixi
	`requires` armmp

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

module Propellor.Property.Systemd.Journald where
import Propellor
import qualified Propellor.Property.Systemd as Systemd
import Utility.DataUnits

-- | Configures journald, restarting it so the changes take effect.
configured :: Systemd.Option -> String -> Property
configured option value =
	Systemd.configured "/etc/systemd/journald.conf" option value
		`onChange` Systemd.restarted "systemd-journald"

-- The string is parsed to get a data size.
-- Examples: "100 megabytes" or "0.5tb"
type DataSize = String

configuredSize :: Systemd.Option -> DataSize -> Property
configuredSize option s = case readSize dataUnits s of
	Just sz -> configured option (systemdSizeUnits sz)
	Nothing -> property ("unable to parse " ++ option ++ " data size " ++ s) noChange

systemMaxUse :: DataSize -> Property
systemMaxUse = configuredSize "SystemMaxUse"

runtimeMaxUse :: DataSize -> Property
runtimeMaxUse = configuredSize "RuntimeMaxUse"

systemKeepFree :: DataSize -> Property
systemKeepFree = configuredSize "SystemKeepFree"

runtimeKeepFree :: DataSize -> Property
runtimeKeepFree = configuredSize "RuntimeKeepFree"

systemMaxFileSize :: DataSize -> Property
systemMaxFileSize = configuredSize "SystemMaxFileSize"

runtimeMaxFileSize :: DataSize -> Property
runtimeMaxFileSize = configuredSize "RuntimeMaxFileSize"

-- Generates size units as used in journald.conf.
systemdSizeUnits :: Integer -> String
systemdSizeUnits sz = filter (/= ' ') (roughSize cfgfileunits True sz)
  where
	cfgfileunits :: [Unit]
	cfgfileunits =
	        [ Unit (p 6) "E" "exabyte"
		, Unit (p 5) "P" "petabyte"
		, Unit (p 4) "T" "terabyte"
		, Unit (p 3) "G" "gigabyte"
		, Unit (p 2) "M" "megabyte"
		, Unit (p 1) "K" "kilobyte"
		]
        p :: Integer -> Integer
        p n = 1024^n

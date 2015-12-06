module Propellor.Property.Journald where

import Propellor.Base
import qualified Propellor.Property.Systemd as Systemd
import Utility.DataUnits

-- | Configures journald, restarting it so the changes take effect.
configured :: Systemd.Option -> String -> Property NoInfo
configured option value =
	Systemd.configured "/etc/systemd/journald.conf" option value
		`onChange` Systemd.restarted "systemd-journald"

-- The string is parsed to get a data size.
-- Examples: "100 megabytes" or "0.5tb"
type DataSize = String

configuredSize :: Systemd.Option -> DataSize -> Property NoInfo
configuredSize option s = case readSize dataUnits s of
	Just sz -> configured option (systemdSizeUnits sz)
	Nothing -> property ("unable to parse " ++ option ++ " data size " ++ s) $
		return FailedChange

systemMaxUse :: DataSize -> Property NoInfo
systemMaxUse = configuredSize "SystemMaxUse"

runtimeMaxUse :: DataSize -> Property NoInfo
runtimeMaxUse = configuredSize "RuntimeMaxUse"

systemKeepFree :: DataSize -> Property NoInfo
systemKeepFree = configuredSize "SystemKeepFree"

runtimeKeepFree :: DataSize -> Property NoInfo
runtimeKeepFree = configuredSize "RuntimeKeepFree"

systemMaxFileSize :: DataSize -> Property NoInfo
systemMaxFileSize = configuredSize "SystemMaxFileSize"

runtimeMaxFileSize :: DataSize -> Property NoInfo
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

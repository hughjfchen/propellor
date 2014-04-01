module Propellor.Property.Cron where

import Propellor
import qualified Propellor.Property.File as File
import qualified Propellor.Property.Apt as Apt

type CronTimes = String

-- | Installs a cron job to run propellor.
runPropellor :: CronTimes -> Property
runPropellor times = "/etc/cron.d/propellor" `File.hasContent`
	[ "# Run propellor"
	, ""
	, "SHELL=/bin/sh"
	, "PATH=/usr/local/sbin:/usr/local/bin:/sbin:/bin:/usr/sbin:/usr/bin"
	, ""
	, times ++ "\troot\tcd " ++ localdir ++ " && nice ionice -c 3 chronic make"
	]
	`requires` Apt.installed ["util-linux", "cron", "moreutils"]
	`describe` "cronned propeller"

module Propellor.Property.Cron where

import Propellor
import qualified Propellor.Property.File as File
import qualified Propellor.Property.Apt as Apt
import Propellor.Bootstrap
import Utility.SafeCommand
import Utility.FileMode

import Data.Char

-- | When to run a cron job.
--
-- The Daily, Monthly, and Weekly options allow the cron job to be run
-- by anacron, which is useful for non-servers.
data Times
	= Times String -- ^ formatted as in crontab(5)
	| Daily
	| Weekly
	| Monthly

-- | Installs a cron job, that will run as a specified user in a particular
-- directory. Note that the Desc must be unique, as it is used for the
-- cron job filename.
-- 
-- Only one instance of the cron job is allowed to run at a time, no matter
-- how long it runs. This is accomplished using flock locking of the cron
-- job file.
--
-- The cron job's output will only be emailed if it exits nonzero.
job :: Desc -> Times -> UserName -> FilePath -> String -> Property NoInfo
job desc times user cddir command = combineProperties ("cronned " ++ desc)
	[ cronjobfile `File.hasContent`
		[ case times of
			Times _ -> ""
			_ -> "#!/bin/sh\nset -e"
		, "# Generated by propellor"
		, ""
		, "SHELL=/bin/sh"
		, "PATH=/usr/local/sbin:/usr/local/bin:/sbin:/bin:/usr/sbin:/usr/bin"
		, ""
		, case times of
			Times t -> t ++ "\t" ++ user ++ "\tchronic " ++ shellEscape scriptfile
			_ -> case user of
				"root" -> "chronic " ++ shellEscape scriptfile
				_ -> "chronic su " ++ user ++ " -c " ++ shellEscape scriptfile
		]
	, case times of
		Times _ -> doNothing
		_ -> cronjobfile `File.mode` combineModes (readModes ++ executeModes)
	-- Use a separate script because it makes the cron job name 
	-- prettier in emails, and also allows running the job manually.
	, scriptfile `File.hasContent`
		[ "#!/bin/sh"
		, "# Generated by propellor"
		, "set -e"
		, "flock -n " ++ shellEscape cronjobfile
			++ " sh -c " ++ shellEscape cmdline
		]
	, scriptfile `File.mode` combineModes (readModes ++ executeModes)
	]
	`requires` Apt.serviceInstalledRunning "cron"
	`requires` Apt.installed ["util-linux", "moreutils"]
  where
	cmdline = "cd " ++ cddir ++ " && ( " ++ command ++ " )"
	cronjobfile = "/etc" </> cronjobdir </> name
	cronjobdir = case times of
		Times _ -> "cron.d"
		Daily -> "cron.daily"
		Weekly -> "cron.weekly"
		Monthly -> "cron.monthly"
	scriptfile = "/usr/local/bin/" ++ name ++ "_cronjob"
	name = map sanitize desc
	sanitize c
		| isAlphaNum c = c
		| otherwise = '_'

-- | Installs a cron job, and runs it niced and ioniced.
niceJob :: Desc -> Times -> UserName -> FilePath -> String -> Property NoInfo
niceJob desc times user cddir command = job desc times user cddir
	("nice ionice -c 3 sh -c " ++ shellEscape command)

-- | Installs a cron job to run propellor.
runPropellor :: Times -> Property NoInfo
runPropellor times = niceJob "propellor" times "root" localdir
	(bootstrapPropellorCommand ++ "; ./propellor")

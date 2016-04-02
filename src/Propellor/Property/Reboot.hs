module Propellor.Property.Reboot where

import Propellor.Base

now :: Property Linux
now = tightenTargets $ cmdProperty "reboot" []
	`assume` MadeChange
	`describe` "reboot now"

-- | Schedules a reboot at the end of the current propellor run.
--
-- The `Result` code of the entire propellor run can be checked;
-- the reboot proceeds only if the function returns True.
--
-- The reboot can be forced to run, which bypasses the init system. Useful
-- if the init system might not be running for some reason.
atEnd :: Bool -> (Result -> Bool) -> Property Linux
atEnd force resultok = property "scheduled reboot at end of propellor run" $ do
	endAction "rebooting" atend
	return NoChange
  where
	atend r
		| resultok r = liftIO $ toResult
			<$> boolSystem "reboot" rebootparams
		| otherwise = do
			warningMessage "Not rebooting, due to status of propellor run."
			return FailedChange
	rebootparams
		| force = [Param "--force"]
		| otherwise = []

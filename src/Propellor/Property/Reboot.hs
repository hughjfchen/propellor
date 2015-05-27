module Propellor.Property.Reboot where

import Propellor

now :: Property NoInfo
now = cmdProperty "reboot" []
	`describe` "reboot now"

-- | Schedules a reboot at the end of the current propellor run.
--
-- The Result code of the endire propellor run can be checked;
-- the reboot proceeds only if the function returns True.
--
-- The reboot can be forced to run, which bypasses the init system. Useful
-- if the init system might not be running for some reason.
atEnd :: Bool -> (Result -> Bool) -> Property NoInfo
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

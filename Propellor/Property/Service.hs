module Propellor.Property.Service where

import Propellor
import Utility.SafeCommand

type ServiceName = String

-- | Ensures that a service is running. Does not ensure that
-- any package providing that service is installed. See
-- Apt.serviceInstalledRunning
--
-- Note that due to the general poor state of init scripts, the best
-- we can do is try to start the service, and if it fails, assume
-- this means it's already running.
running :: ServiceName -> Property
running svc = property ("running " ++ svc) $ do
	void $ ensureProperty $
		scriptProperty ["service " ++ shellEscape svc ++ " start >/dev/null 2>&1 || true"]
	return NoChange

restarted :: ServiceName -> Property
restarted svc = property ("restarted " ++ svc) $ do
	void $ ensureProperty $
		scriptProperty ["service " ++ shellEscape svc ++ " restart >/dev/null 2>&1 || true"]
	return NoChange

reloaded :: ServiceName -> Property
reloaded svc = property ("reloaded " ++ svc) $ do
	void $ ensureProperty $
		scriptProperty ["service " ++ shellEscape svc ++ " reload >/dev/null 2>&1 || true"]
	return NoChange

module Propellor.Property.Service where

import Propellor.Base

type ServiceName = String

-- | Ensures that a service is running. Does not ensure that
-- any package providing that service is installed. See
-- Apt.serviceInstalledRunning
--
-- Note that due to the general poor state of init scripts, the best
-- we can do is try to start the service, and if it fails, assume
-- this means it's already running.
running :: ServiceName -> Property NoInfo
running = signaled "start" "running"

restarted :: ServiceName -> Property NoInfo
restarted = signaled "restart" "restarted"

reloaded :: ServiceName -> Property NoInfo
reloaded = signaled "reload" "reloaded"

signaled :: String -> Desc -> ServiceName -> Property NoInfo
signaled cmd desc svc = p `describe` (desc ++ " " ++ svc)
  where
	p = scriptProperty ["service " ++ shellEscape svc ++ " " ++ cmd ++ " >/dev/null 2>&1 || true"]
		`assume` NoChange

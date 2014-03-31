module Propellor.Property.Docker where

import Propellor
import qualified Propellor.Property.File as File
import qualified Propellor.Property.Apt as Apt

{- | Configures docker with an authentication file, so that images can be
 - pushed to index.docker.io. -}
configured :: Property
configured = Property "docker configured" go `requires` installed
  where
	go = withPrivData DockerAuthentication $ \cfg -> ensureProperty $ 
		"/root/.dockercfg" `File.hasContent` (lines cfg)
	
installed :: Property
installed = Apt.installed ["docker.io"]

module Property.Docker where

import Common
import qualified Property.File as File
import qualified Property.Apt as Apt

{- Configures docker with an authentication file, so that images can be
 - pushed to index.docker.io. -}
configured :: Property
configured = Property "docker configured" go `requires` installed
  where
	go = withPrivData DockerAuthentication $ \cfg -> ensureProperty $ 
		"/root/.dockercfg" `File.hasContent` (lines cfg)
	
installed :: Property
installed = Apt.installed ["docker.io"]

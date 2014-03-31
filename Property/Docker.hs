module Property.Docker where

import Common
import qualified Property.File as File

{- Configures docker with an authentication file, so that images can be
 - pushed to index.docker.io. -}
configured :: Property
configured = Property "docker configured" $ 
	withPrivData DockerAuthentication $ \cfg ->
		ensureProperty $ "/root/.dockercfg" `File.hasContent` (lines cfg)

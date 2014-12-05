module Propellor.Property.Chroot.Util where

import Utility.Env
import Control.Applicative

-- When chrooting, it's useful to ensure that PATH has all the standard
-- directories in it. This adds those directories to whatever PATH is
-- already set.
standardPathEnv :: IO [(String, String)]
standardPathEnv = do
	path <- getEnvDefault "PATH" "/bin"
	addEntry "PATH" (path ++ stdPATH)
		<$> getEnvironment

stdPATH :: String
stdPATH = "/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin"

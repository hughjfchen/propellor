-- This is the main configuration file for Propellor, and is used to build
-- the propellor program.

import Propellor
import Propellor.Engine
import qualified Propellor.Property.User as User

main :: IO ()
main = mainProperties localBox

-- An local host which should satisfy some properties.
localBox :: Host
localBox =
  host "local" $
    props
      & osCentOS (CentOSLinux CentOS7) X86_64
      & User.hasSomePassword (User "ubuntu")

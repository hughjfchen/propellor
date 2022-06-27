-- This is the main configuration file for Propellor, and is used to build
-- the propellor program.

import Propellor
import Propellor.Engine
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.Cron as Cron
import qualified Propellor.Property.File as File
import qualified Propellor.Property.User as User

main :: IO ()
main = mainProperties localBox

-- An local host which should satisfy some properties.
localBox :: Host
localBox =
  host "local" $
    props
      & osBuntish "20.04" X86_64
      & User.hasSomePassword (User "chenjf")

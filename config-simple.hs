-- This is the main configuration file for Propellor, and is used to build
-- the propellor program.

import Propellor
import qualified Propellor.Property.Bootstrap as Bootstrap
import qualified Propellor.Property.User as User

main :: IO ()
-- main = mainProperties localBox
main =
  defaultMain
    [ lxcCentOS7
    ]

-- An local host which should satisfy some properties.
lxcCentOS7 :: Host
lxcCentOS7 =
  host "lxc-centos7" $
    props
      & osCentOS (CentOSLinux CentOS7) X86_64
      & Bootstrap.bootstrapWith (Bootstrap.Robustly Bootstrap.Stack)
      & User.accountFor (User "chenjf")
      & User.systemAccountFor (User "systemAcc1")
      & User.systemGroup (Group "systemGrp1")

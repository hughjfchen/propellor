-- This is the main configuration file for Propellor, and is used to build
-- the propellor program.

import Propellor
import qualified Propellor.Property.Bootstrap as Bootstrap
import qualified Propellor.Property.Cmd as Cmd
import qualified Propellor.Property.File as File
import qualified Propellor.Property.Systemd as Systemd

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
      & Bootstrap.bootstrapWith (Bootstrap.Robustly Bootstrap.Nix)
      & Cmd.cmdProperty "userdel" ["-r", "systemAcc1"] `assume` MadeChange
      & File.hasContent "/root/test-file" ["Should be there"]
      & Systemd.daemonReloaded

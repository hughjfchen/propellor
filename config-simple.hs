-- This is the main configuration file for Propellor, and is used to build
-- the propellor program.

import Propellor
import qualified Propellor.Property.File as File
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.Cron as Cron
import qualified Propellor.Property.User as User

import System.FilePath
import Propellor.Types.Core
import qualified Propellor.Property.Chroot as Chroot
import Propellor.Property.Chroot
import qualified Propellor.Property.Mount as Mount
import Control.Monad.IO.Class

main :: IO ()
main = defaultMain hosts

-- The hosts propellor knows about.
hosts :: [Host]
hosts =
	[ mybox
	]

-- An example host.
mybox :: Host
mybox = host "mybox.example.com" $ props
	& osDebian Unstable "amd64"
	& Apt.stdSourcesList
	& Apt.unattendedUpgrades
	& Apt.installed ["etckeeper"]
	& Apt.installed ["ssh"]
	& User.hasSomePassword (User "root")
	& File.dirExists "/var/www"
	& Cron.runPropellor (Cron.Times "30 * * * *")

schroot :: String -> Chroot -> RevertableProperty (HasInfo + DebianLike) DebianLike
schroot sn chroot@(Chroot.Chroot chrootdir _ _) = (setup `requires` installed) <!> cleanup
        where
          setup :: Property (HasInfo + DebianLike)
          setup = conf `requires` (provision `onChange` targz)
              where
                provision :: Property (HasInfo + DebianLike)
                provision = setupRevertableProperty (Chroot.provisioned chroot) `before` umount
                    where
                      umount :: Property Linux
                      umount = property ("umount " ++ chrootdir) $ do
                                 liftIO $ Mount.unmountBelow chrootdir
                                 return NoChange
          cleanup :: Property DebianLike
          cleanup = File.notPresent (schrootChrootD </> sn)
                    `requires` File.notPresent tarball
                    `requires` revert (Chroot.provisioned chroot)
          tarball = chrootdir <.> "tar.gz"
	  -- dummy stuff added to make it compile as I don't have the real
	  -- stuff handy.
	  installed = undefined :: Property DebianLike
	  conf = undefined :: Property DebianLike
	  targz = undefined :: Property DebianLike
	  schrootChrootD = undefined :: FilePath


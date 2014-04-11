-- | This is the main configuration file for Propellor, and is used to build
-- the propellor program.

import Propellor
import Propellor.CmdLine
import Propellor.Property.Scheduled
import qualified Propellor.Property.File as File
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.Network as Network
--import qualified Propellor.Property.Ssh as Ssh
import qualified Propellor.Property.Cron as Cron
--import qualified Propellor.Property.Sudo as Sudo
import qualified Propellor.Property.User as User
--import qualified Propellor.Property.Hostname as Hostname
--import qualified Propellor.Property.Reboot as Reboot
--import qualified Propellor.Property.Tor as Tor
import qualified Propellor.Property.Docker as Docker

-- The hosts propellor knows about.
-- Edit this to configure propellor!
hosts :: [Host]
hosts =
	[ host "mybox.example.com"
		& Apt.stdSourcesList Unstable
			`onChange` Apt.upgrade
		& Apt.unattendedUpgrades
		& Apt.installed ["etckeeper"]
		& Apt.installed ["ssh"]
		& User.hasSomePassword "root"
		& Network.ipv6to4
		& File.dirExists "/var/www"
		& Docker.docked hosts "webserver"
		& Docker.garbageCollected `period` Daily
		& Cron.runPropellor "30 * * * *"

	-- A generic webserver in a Docker container.
	, Docker.container "webserver" "joeyh/debian-unstable"
		& Docker.publish "80:80"
		& Docker.volume "/var/www:/var/www"
		& Apt.serviceInstalledRunning "apache2"

	-- add more hosts here...
	--, host "foo.example.com" = ...
	]

main :: IO ()
main = defaultMain hosts

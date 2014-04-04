-- | This is the main configuration file for Propellor, and is used to build
-- the propellor program.

module Propellor.Config.Simple where

import Propellor
import Propellor.CmdLine
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

main :: IO ()
main = defaultMain [host, Docker.containerProperties container]

-- | This is where the system's HostName, either as returned by uname
-- or one specified on the command line, is converted into a list of
-- Properties for that system.
--
-- Edit this to configure propellor!
host :: HostName -> Maybe [Property]
host hostname@"mybox.example.com" = Just $ props
	& Apt.stdSourcesList Unstable
		`onChange` Apt.upgrade
	& Apt.unattendedUpgrades
	& Apt.installed ["etckeeper"]
	& Apt.installed ["ssh"]
	& User.hasSomePassword "root"
	& Network.ipv6to4
	& File.dirExists "/var/www"
	& Docker.docked container hostname "webserver"
	& Docker.garbageCollected
	& Cron.runPropellor "30 * * * *"
-- add more hosts here...
--host "foo.example.com" =
host _ = Nothing

-- | This is where Docker containers are set up. A container
-- can vary by hostname where it's used, or be the same everywhere.
container :: HostName -> Docker.ContainerName -> Maybe (Docker.Container)
container _ "webserver" = Just $ Docker.containerFrom "joeyh/debian-unstable"
	[ Docker.publish "80:80"
	, Docker.volume "/var/www:/var/www"
	, Docker.inside $ props
		& serviceRunning "apache2"
			`requires` Apt.installed ["apache2"]
	]
container _ _ = Nothing

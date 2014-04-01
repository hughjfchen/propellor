-- | This is the main configuration file for Propellor, and is used to build
-- the propellor program.

import Propellor
import Propellor.CmdLine
import qualified Propellor.Property.File as File
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.Network as Network
import qualified Propellor.Property.Ssh as Ssh
import qualified Propellor.Property.Cron as Cron
import qualified Propellor.Property.Sudo as Sudo
import qualified Propellor.Property.User as User
import qualified Propellor.Property.Hostname as Hostname
import qualified Propellor.Property.Reboot as Reboot
import qualified Propellor.Property.Tor as Tor
import qualified Propellor.Property.Docker as Docker
import qualified Propellor.Property.GitHome as GitHome
import qualified Propellor.Property.JoeySites as JoeySites

main :: IO ()
main = defaultMain [host, Docker.containerProperties container]

-- | This is where the system's HostName, either as returned by uname
-- or one specified on the command line, is converted into a list of
-- Properties for that system.
--
-- Edit this to configure propellor!
host :: HostName -> Maybe [Property]
host hostname@"clam.kitenet.net" = Just
	[ cleanCloudAtCost hostname
	, standardSystem Apt.Unstable
	, Apt.unattendedUpgrades True
	, Network.ipv6to4
	-- Clam is a tor bridge, and an olduse.net shellbox and other
	-- fun stuff.
	, Tor.isBridge
	, JoeySites.oldUseNetshellBox
	, Docker.configured
	, File.dirExists "/var/www"
	, Docker.hasContainer hostname "webserver" container
	, Apt.installed ["git-annex", "mtr"]
	-- Should come last as it reboots.
	, Apt.installed ["systemd-sysv"] `onChange` Reboot.now
	]
host "orca.kitenet.net" = Just
	[ standardSystem Apt.Unstable
	, Apt.unattendedUpgrades True
	, Docker.configured
	]
-- add more hosts here...
--host "foo.example.com" =
host _ = Nothing

-- | This is where Docker containers are set up. A container
-- can vary by hostname where it's used, or be the same everywhere.
container :: HostName -> Docker.ContainerName -> Maybe (Docker.Container)
container _ "webserver" = Just $ Docker.containerFromImage "debian"
	[ Docker.publish "80:80"
	, Docker.volume "/var/www:/var/www"
	, Docker.inside
		[ serviceRunning "apache2"
			`requires` Apt.installed ["apache2"]
		]
	]
container _ _ = Nothing

-- This is my standard system setup
standardSystem :: Apt.Suite -> Property
standardSystem suite = propertyList "standard system"
	[ Apt.stdSourcesList suite `onChange` Apt.upgrade
	, Apt.installed ["etckeeper"]
	, Apt.installed ["ssh"]
	, GitHome.installedFor "root"
	, User.hasSomePassword "root"
	-- Harden the system, but only once root's authorized_keys
	-- is safely in place.
	, check (Ssh.hasAuthorizedKeys "root") $
		Ssh.passwordAuthentication False
	, User.sshAccountFor "joey"
	, User.hasSomePassword "joey"
	, Sudo.enabledFor "joey"
	, GitHome.installedFor "joey"
	, Apt.installed ["vim", "screen"]
	, Cron.runPropellor "30 * * * *"
	-- I use postfix, or no MTA.
	, Apt.removed ["exim4"] `onChange` Apt.autoRemove
	]

-- Clean up a system as installed by cloudatcost.com
cleanCloudAtCost :: HostName -> Property
cleanCloudAtCost hostname = propertyList "cloudatcost cleanup"
	[ Hostname.set hostname
	, Ssh.uniqueHostKeys
	, "worked around grub/lvm boot bug #743126" ==>
		"/etc/default/grub" `File.containsLine` "GRUB_DISABLE_LINUX_UUID=true"
		`onChange` cmdProperty "update-grub" []
		`onChange` cmdProperty "update-initramfs" ["-u"]
	, "nuked cloudatcost cruft" ==> combineProperties
		[ File.notPresent "/etc/rc.local"
		, File.notPresent "/etc/init.d/S97-setup.sh"
		, User.nuked "user" User.YesReallyDeleteHome
		]
	]

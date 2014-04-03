-- | This is the main configuration file for Propellor, and is used to build
-- the propellor program.
--
-- This is the live config file used by propellor's author.
-- For a simpler starting point, see simple-config.hs

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
import qualified Propellor.Property.SiteSpecific.GitHome as GitHome
import qualified Propellor.Property.SiteSpecific.GitAnnexBuilder as GitAnnexBuilder
import qualified Propellor.Property.SiteSpecific.JoeySites as JoeySites
import Data.List

main :: IO ()
main = defaultMain [host, Docker.containerProperties container]

-- | This is where the system's HostName, either as returned by uname
-- or one specified on the command line, is converted into a list of
-- Properties for that system.
--
-- Edit this to configure propellor!
host :: HostName -> Maybe [Property]
host hostname@"clam.kitenet.net" = Just $ props
	& cleanCloudAtCost hostname
	& standardSystem Unstable
	& Apt.unattendedUpgrades
	& Network.ipv6to4
	& Apt.installed ["git-annex", "mtr"]
	-- Clam is a tor bridge, and an olduse.net shellbox and other
	-- fun stuff.
	& Tor.isBridge
	& JoeySites.oldUseNetshellBox
	& Docker.configured
	& File.dirExists "/var/www"
	! Docker.docked container hostname "webserver"
	! Docker.docked container hostname "amd64-git-annex-builder"
	& Docker.garbageCollected
	-- Should come last as it reboots.
	& Apt.installed ["systemd-sysv"] `onChange` Reboot.now
host hostname@"orca.kitenet.net" = Just $ props
	& Hostname.set hostname
	& standardSystem Unstable
	& Apt.unattendedUpgrades
	& Docker.configured
	! Docker.docked container hostname "amd64-git-annex-builder"
	! Docker.docked container hostname "i386-git-annex-builder"
	& Docker.garbageCollected
-- add more hosts here...
--host "foo.example.com" =
host _ = Nothing

-- | This is where Docker containers are set up. A container
-- can vary by hostname where it's used, or be the same everywhere.
container :: HostName -> Docker.ContainerName -> Maybe (Docker.Container)
container _host name
	| name == "webserver" = Just $ Docker.containerFrom
		(image $ System (Debian Unstable) "amd64")
		[ Docker.publish "8080:80"
		, Docker.volume "/var/www:/var/www"
		, Docker.inside $ props
			& serviceRunning "apache2"
				`requires` Apt.installed ["apache2"]
		]
	| "-git-annex-builder" `isSuffixOf` name =
		let arch = takeWhile (/= '-') name
		in Just $ Docker.containerFrom
			(image $ System (Debian Unstable) arch)
			[ Docker.inside $ props & GitAnnexBuilder.builder arch "15 * * * *" True ]
	| otherwise = Nothing

-- | Docker images I prefer to use.
-- Edit as suites you, or delete this function and just put the image names
-- above.
image :: System -> Docker.Image
image (System (Debian Unstable) "amd64") = "joeyh/debian-unstable"
image (System (Debian Unstable) "i386") = "joeyh/debian-unstable-i386"
image _ = "debian"

-- This is my standard system setup
standardSystem :: DebianSuite -> Property
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
	, User.accountFor "joey"
	, User.hasSomePassword "joey"
	, Sudo.enabledFor "joey"
	, GitHome.installedFor "joey"
	, Apt.installed ["vim", "screen", "less"]
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
	, combineProperties "nuked cloudatcost cruft"
		[ File.notPresent "/etc/rc.local"
		, File.notPresent "/etc/init.d/S97-setup.sh"
		, User.nuked "user" User.YesReallyDeleteHome
		]
	]

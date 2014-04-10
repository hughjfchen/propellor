-- |  This is the live config file used by propellor's author.

import Propellor
import Propellor.CmdLine
import Propellor.Property.Scheduled
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
import qualified Propellor.Property.Dns as Dns
import qualified Propellor.Property.OpenId as OpenId
import qualified Propellor.Property.Docker as Docker
import qualified Propellor.Property.Git as Git
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
-- Clam is a tor bridge, and an olduse.net shellbox and other fun stuff.
host hostname@"clam.kitenet.net" = Just $ withSystemd $ props
	& cleanCloudAtCost hostname
	& standardSystem Unstable
	& Apt.unattendedUpgrades
	& Network.ipv6to4
	& Apt.installed ["git-annex", "mtr"]
	& Tor.isBridge
	& JoeySites.oldUseNetshellBox
	& Docker.docked container hostname "openid-provider"
		`requires` Apt.installed ["ntp"]
	& Docker.docked container hostname "ancient.kitenet.net"
	& Docker.configured
	& Docker.garbageCollected `period` Daily
-- Orca is the main git-annex build box.
host hostname@"orca.kitenet.net" = Just $ props -- no systemd due to #726375
	& standardSystem Unstable
	& Hostname.set hostname
	& Apt.unattendedUpgrades
	& Docker.configured
	& Apt.buildDep ["git-annex"] `period` Daily
	& Docker.docked container hostname "amd64-git-annex-builder"
	& Docker.docked container hostname "i386-git-annex-builder"
	! Docker.docked container hostname "armel-git-annex-builder-companion"
	! Docker.docked container hostname "armel-git-annex-builder"
	& Docker.garbageCollected `period` Daily
-- Diatom is my downloads and git repos server, and secondary dns server.
host hostname@"diatom.kitenet.net" = Just $ props
	& standardSystem Stable
	& Hostname.set hostname
	& Apt.unattendedUpgrades
	& Apt.serviceInstalledRunning "ntp"
	& Dns.zones myDnsSecondary
	& Apt.serviceInstalledRunning "apache2"
	& Apt.installed ["git", "git-annex", "rsync"]
	& Apt.buildDep ["git-annex"] `period` Daily
	& Git.daemonRunning "/srv/git"
	& File.ownerGroup "/srv/git" "joey" "joey"
	-- git repos restore (how?)
	-- family annex needs family members to have accounts,
	--     ssh host key etc.. finesse?
	--   (also should upgrade git-annex-shell for it..)
	-- kgb installation and setup
	-- ssh keys for branchable and github repo hooks
	-- gitweb
	-- downloads.kitenet.net setup (including ssh key to turtle)
-- My laptop
host _hostname@"darkstar.kitenet.net" = Just $ props
	& Docker.configured
	& Apt.buildDep ["git-annex"] `period` Daily
	
-- add more hosts here...
--host "foo.example.com" =
host _ = Nothing

-- | This is where Docker containers are set up. A container
-- can vary by hostname where it's used, or be the same everywhere.
container :: HostName -> Docker.ContainerName -> Maybe (Docker.Container)
container _parenthost name
	-- Simple web server, publishing the outside host's /var/www
	| name == "webserver" = Just $ standardContainer Stable "amd64"
		[ Docker.publish "8080:80"
		, Docker.volume "/var/www:/var/www"
		, Docker.inside $ props
			& Apt.serviceInstalledRunning "apache2"
		]

	-- My own openid provider. Uses php, so containerized for security
	-- and administrative sanity.
	| name == "openid-provider" = Just $ standardContainer Stable "amd64"
		[ Docker.publish "8081:80"
		, Docker.inside $ props
			& OpenId.providerFor ["joey", "liw"]
				"openid.kitenet.net:8081"
		]
	
	| name == "ancient.kitenet.net" = Just $ standardContainer Stable "amd64"
		[ Docker.publish "1994:80"
		, Docker.inside $ props
			& Apt.serviceInstalledRunning "apache2"
			& Apt.installed ["git"]
			& scriptProperty 
				[ "cd /var/"
				, "rm -rf www"
				, "git clone git://git.kitenet.net/git/kitewiki www"
				, "git checkout remotes/origin/old-kitenet.net"
				] `flagFile` "/var/www/blastfromthepast.html"
		]
	
	-- armel builder has a companion container that run amd64 and
	-- runs the build first to get TH splices. They share a home
	-- directory, and need to have the same versions of all haskell
	-- libraries installed.
	| name == "armel-git-annex-builder-companion" = Just $ Docker.containerFrom
		(image $ System (Debian Unstable) "amd64")
		[ Docker.volume GitAnnexBuilder.homedir
		, Docker.inside $ props
			& Apt.unattendedUpgrades
		]
	| name == "armel-git-annex-builder" = Just $ Docker.containerFrom
		(image $ System (Debian Unstable) "armel")
		[ Docker.link (name ++ "-companion") "companion"
		, Docker.volumes_from (name ++ "-companion")
		, Docker.inside $ props
--			& GitAnnexBuilder.builder "armel" "15 * * * *" True
			& Apt.unattendedUpgrades
		]
	
	| "-git-annex-builder" `isSuffixOf` name =
		let arch = takeWhile (/= '-') name
		in Just $ Docker.containerFrom
			(image $ System (Debian Unstable) arch)
			[ Docker.inside $ props
				& GitAnnexBuilder.builder arch "15 * * * *" True
				& Apt.unattendedUpgrades
			]
	
	| otherwise = Nothing

-- | Docker images I prefer to use.
image :: System -> Docker.Image
image (System (Debian Unstable) arch) = "joeyh/debian-unstable-" ++ arch
image (System (Debian Stable) arch) = "joeyh/debian-stable-" ++ arch
image _ = "debian-stable-official" -- does not currently exist!

-- This is my standard system setup
standardSystem :: DebianSuite -> Property
standardSystem suite = propertyList "standard system" $ props
	& Apt.stdSourcesList suite `onChange` Apt.upgrade
	& Apt.installed ["etckeeper"]
	& Apt.installed ["ssh"]
	& GitHome.installedFor "root"
	& User.hasSomePassword "root"
	-- Harden the system, but only once root's authorized_keys
	-- is safely in place.
	& check (Ssh.hasAuthorizedKeys "root")
		(Ssh.passwordAuthentication False)
	& User.accountFor "joey"
	& User.hasSomePassword "joey"
	& Sudo.enabledFor "joey"
	& GitHome.installedFor "joey"
	& Apt.installed ["vim", "screen", "less"]
	& Cron.runPropellor "30 * * * *"
	-- I use postfix, or no MTA.
	& Apt.removed ["exim4", "exim4-daemon-light", "exim4-config", "exim4-base"]
		`onChange` Apt.autoRemove

withSystemd :: [Property] -> [Property]
withSystemd ps = ps ++ [Apt.installed ["systemd-sysv"] `onChange` Reboot.now]

-- This is my standard container setup, featuring automatic upgrades.
standardContainer :: DebianSuite -> Architecture -> [Docker.Containerized Property] -> Docker.Container
standardContainer suite arch ps = Docker.containerFrom
	(image $ System (Debian suite) arch) $
	[ Docker.inside $ props
		& Apt.stdSourcesList suite
		& Apt.unattendedUpgrades
	] ++ ps

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

myDnsSecondary :: [Dns.Zone]
myDnsSecondary =
	[ Dns.secondary "kitenet.net" master
	, Dns.secondary "joeyh.name" master
	, Dns.secondary "ikiwiki.info" master
	, Dns.secondary "olduse.net" master
	, Dns.secondary "branchable.com" branchablemaster
	]
  where
	master = ["80.68.85.49", "2001:41c8:125:49::10"] -- wren
	branchablemaster = ["66.228.46.55", "2600:3c03::f03c:91ff:fedf:c0e5"]

-- This is the live config file used by propellor's author.
-- https://propellor.branchable.com/
module Main where

import Propellor
import Propellor.CmdLine
import Propellor.Property.Scheduled
import qualified Propellor.Property.File as File
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.Network as Network
import qualified Propellor.Property.Service as Service
import qualified Propellor.Property.Ssh as Ssh
import qualified Propellor.Property.Gpg as Gpg
import qualified Propellor.Property.Cron as Cron
import qualified Propellor.Property.Sudo as Sudo
import qualified Propellor.Property.User as User
import qualified Propellor.Property.Hostname as Hostname
import qualified Propellor.Property.Tor as Tor
import qualified Propellor.Property.Dns as Dns
import qualified Propellor.Property.OpenId as OpenId
import qualified Propellor.Property.Docker as Docker
import qualified Propellor.Property.Git as Git
import qualified Propellor.Property.Apache as Apache
import qualified Propellor.Property.Postfix as Postfix
import qualified Propellor.Property.Grub as Grub
import qualified Propellor.Property.Obnam as Obnam
import qualified Propellor.Property.HostingProvider.DigitalOcean as DigitalOcean
import qualified Propellor.Property.HostingProvider.CloudAtCost as CloudAtCost
import qualified Propellor.Property.HostingProvider.Linode as Linode
import qualified Propellor.Property.SiteSpecific.GitHome as GitHome
import qualified Propellor.Property.SiteSpecific.GitAnnexBuilder as GitAnnexBuilder
import qualified Propellor.Property.SiteSpecific.JoeySites as JoeySites


main :: IO ()           --     _         ______`|                       ,-.__ 
main = defaultMain hosts --  /   \___-=O`/|O`/__|                      (____.'
  {- Propellor            -- \          / | /    )          _.-"-._
     Deployed -}          --  `/-==__ _/__|/__=-|          (       \_
hosts :: [Host]          --   *             \ | |           '--------'
hosts =                --                  (o)  `
	[ darkstar
	, clam
	, orca
	, kite
	, diatom
	, elephant
	] ++ containers ++ monsters

darkstar :: Host
darkstar = host "darkstar.kitenet.net"
	& ipv6 "2001:4830:1600:187::2" -- sixxs tunnel

	& Apt.buildDep ["git-annex"] `period` Daily
	& Docker.configured
	! Docker.docked hosts "android-git-annex"

clam :: Host
clam = standardSystem "clam.kitenet.net" Unstable "amd64"
	[ "Unreliable server. Anything here may be lost at any time!" ]
	& ipv4 "162.248.9.29"

	& CloudAtCost.decruft
	& Apt.unattendedUpgrades
	& Network.ipv6to4
	& Tor.isBridge
	& Postfix.satellite

	& Docker.configured
	& Docker.garbageCollected `period` Daily
	& Docker.docked hosts "webserver"
	& File.dirExists "/var/www/html"
	& File.notPresent "/var/www/html/index.html"
	& "/var/www/index.html" `File.hasContent` ["hello, world"]
	& alias "helloworld.kitenet.net"
	
	-- ssh on some extra ports to deal with horrible networks
	-- while travelling
	& alias "travelling.kitenet.net"
	! Ssh.listenPort 80
	! Ssh.listenPort 443
	
orca :: Host
orca = standardSystem "orca.kitenet.net" Unstable "amd64"
	[ "Main git-annex build box." ]
	& ipv4 "138.38.108.179"

	& Apt.unattendedUpgrades
	& Postfix.satellite
	& Docker.configured
	& Docker.docked hosts "amd64-git-annex-builder"
	& Docker.docked hosts "i386-git-annex-builder"
	& Docker.docked hosts "android-git-annex-builder"
	& Docker.docked hosts "armel-git-annex-builder-companion"
	& Docker.docked hosts "armel-git-annex-builder"
	& Docker.garbageCollected `period` Daily
	& Apt.buildDep ["git-annex"] `period` Daily
	
-- This is not a complete description of kite, since it's a
-- multiuser system with eg, user passwords that are not deployed
-- with propellor.
kite :: Host
kite = standardSystemUnhardened "kite.kitenet.net" Unstable "amd64"
	[ "Welcome to the new kitenet.net server!"
	]
	& ipv4 "66.228.36.95"
	& ipv6 "2600:3c03::f03c:91ff:fe73:b0d2"
	& alias "kitenet.net"
	& alias "wren.kitenet.net" -- temporary

	& Apt.installed ["linux-image-amd64"]
	& Linode.chainPVGrub 5
	& Apt.unattendedUpgrades
	& Apt.installed ["systemd"]
	& Ssh.hostKeys (Context "kitenet.net")
	& Ssh.passwordAuthentication True
	-- Since ssh password authentication is allowed:
	& Apt.serviceInstalledRunning "fail2ban"
	& Obnam.backup "/" "33 1 * * *"
		[ "--repository=sftp://joey@eubackup.kitenet.net/~/lib/backup/kite.obnam"
		, "--client-name=kitenet.net"
		, "--encrypt-with=98147487"
		, "--exclude=/var/cache"
		, "--exclude=/var/tmp"
		, "--exclude=/home/joey/lib"
		, "--exclude=.*/tmp/"
		, "--one-file-system"
		] Obnam.OnlyClient
		`requires` Gpg.keyImported "98147487" "root"
		`requires` Ssh.keyImported SshRsa "root"
			(Context "kite.kitenet.net")
		`requires` Ssh.knownHost hosts "eubackup.kitenet.net" "root"
	& Apt.serviceInstalledRunning "ntp"
	& "/etc/timezone" `File.hasContent` ["US/Eastern"]

	& alias "smtp.kitenet.net"
	& alias "imap.kitenet.net"
	& alias "pop.kitenet.net"
	& alias "mail.kitenet.net"
	& JoeySites.kiteMailServer

	& JoeySites.legacyWebSites

	& alias "bitlbee.kitenet.net"
	& Apt.serviceInstalledRunning "bitlbee"
	& "/etc/bitlbee/bitlbee.conf" `File.hasContent`
		[ "[settings]"
		, "User = bitlbee"
		, "AuthMode = Registered"
		, "[defaults]"
		] 
		`onChange` Service.restarted "bitlbee"
	& "/etc/default/bitlbee" `File.containsLine` "BITLBEE_PORT=\"6767\""
		`onChange` Service.restarted "bitlbee"

	& Apt.installed
		["git-annex", "myrepos"
		, "build-essential", "make"
		, "rss2email", "archivemail"
		, "devscripts"
		-- Some users have zsh as their login shell.
		, "zsh"
		]
	
	& Docker.configured
	& Docker.garbageCollected `period` Daily

diatom :: Host
diatom = standardSystem "diatom.kitenet.net" (Stable "wheezy") "amd64"
	[ "Important stuff that needs not too much memory or CPU." ]
	& ipv4 "107.170.31.195"

	& DigitalOcean.distroKernel
	& Ssh.hostKeys (Context "diatom.kitenet.net")
	& Apt.unattendedUpgrades
	& Apt.serviceInstalledRunning "ntp"
	& Postfix.satellite

	-- Diatom has 500 mb of memory, so tune for that.
	& JoeySites.obnamLowMem
	& Apt.serviceInstalledRunning "swapspace"
	
	& Apt.serviceInstalledRunning "apache2"
	& JoeySites.kitenetHttps
	& Apache.multiSSL
	& File.ownerGroup "/srv/web" "joey" "joey"
	& Apt.installed ["analog"]

	& alias "git.kitenet.net"
	& alias "git.joeyh.name"
	& JoeySites.gitServer hosts
	
	& alias "downloads.kitenet.net"
	& JoeySites.annexWebSite "/srv/git/downloads.git"
		"downloads.kitenet.net"
		"840760dc-08f0-11e2-8c61-576b7e66acfd"
		[("usbackup", "ssh://usbackup.kitenet.net/~/lib/downloads/")]
		`requires` Ssh.keyImported SshRsa "joey" (Context "downloads.kitenet.net")
		`requires` Ssh.knownHost hosts "usbackup.kitenet.net" "joey"
	& JoeySites.gitAnnexDistributor
		& alias "tmp.kitenet.net"
	& JoeySites.annexWebSite "/srv/git/joey/tmp.git"
		"tmp.kitenet.net"
		"26fd6e38-1226-11e2-a75f-ff007033bdba"
		[]
	& JoeySites.twitRss
	& JoeySites.pumpRss
		
	& alias "nntp.olduse.net"
	& alias "resources.olduse.net"
	& JoeySites.oldUseNetServer hosts
	
	& alias "ns2.kitenet.net"
	& myDnsPrimary "kitenet.net" []
	& myDnsPrimary "joeyh.name" []
	& myDnsPrimary "ikiwiki.info" []
	& myDnsPrimary "olduse.net"
		[ (RelDomain "article",
			CNAME $ AbsDomain "virgil.koldfront.dk") ]

	& alias "ns3.branchable.com"
	& branchableSecondary
	
	& Dns.secondaryFor ["animx"] hosts "animx.eu.org"

elephant :: Host
elephant = standardSystem "elephant.kitenet.net" Unstable "amd64"
	[ "Storage, big data, and backups, omnomnom!"
	, "(Encrypt all data stored here.)"
	]
	& ipv4 "193.234.225.114"
		& Grub.chainPVGrub "hd0,0" "xen/xvda1" 30
	& Postfix.satellite
	& Apt.unattendedUpgrades
	& Ssh.hostKeys ctx
	& sshPubKey "ecdsa-sha2-nistp256 AAAAE2VjZHNhLXNoYTItbmlzdHAyNTYAAAAIbmlzdHAyNTYAAABBBAJkoPRhUGT8EId6m37uBdYEtq42VNwslKnc9mmO+89ody066q6seHKeFY6ImfwjcyIjM30RTzEwftuVNQnbEB0="
	& Ssh.keyImported SshRsa "joey" ctx
	& Apt.serviceInstalledRunning "swapspace"
		& alias "eubackup.kitenet.net"
	& Apt.installed ["obnam", "sshfs", "rsync"]
	& JoeySites.obnamRepos ["wren", "pell", "kite"]
	& JoeySites.githubBackup
	& JoeySites.rsyncNetBackup hosts
	& JoeySites.backupsBackedupTo hosts "usbackup.kitenet.net" "lib/backup/eubackup"
		& alias "podcatcher.kitenet.net"
	& JoeySites.podcatcher
	
	& alias "znc.kitenet.net"
	& JoeySites.ircBouncer
	-- I'd rather this were on diatom, but it needs unstable.
	& alias "kgb.kitenet.net"
	& JoeySites.kgbServer
		& alias "mumble.kitenet.net"
	& JoeySites.mumbleServer hosts
	
	& alias "ns3.kitenet.net"
	& myDnsSecondary
	
	& Docker.configured
		& Docker.docked hosts "oldusenet-shellbox"
	& Docker.docked hosts "openid-provider"
		`requires` Apt.serviceInstalledRunning "ntp"
	& Docker.docked hosts "ancient-kitenet"

	& Docker.garbageCollected `period` (Weekly (Just 1))
	
	-- For https port 443, shellinabox with ssh login to
	-- kitenet.net
	& alias "shell.kitenet.net"
	& JoeySites.kiteShellBox
	-- Nothing is using http port 80, so listen on
	-- that port for ssh, for traveling on bad networks that
	-- block 22.
	& Ssh.listenPort 80
  where
	ctx = Context "elephant.kitenet.net"


	    --'                        __|II|      ,.
	  ----                      __|II|II|__   (  \_,/\
	 ------'\o/-'-.-'-.-'-.- __|II|II|II|II|___/   __/ -'-.-'-.-'-.-'-.-'-
	----------------------- |      [Docker]       / ----------------------
	----------------------- :                    / -----------------------
	------------------------ \____, o          ,' ------------------------
	------------------------- '--,___________,'  -------------------------
containers :: [Host]
containers =
	-- Simple web server, publishing the outside host's /var/www
	[ standardStableContainer "webserver"
		& Docker.publish "80:80"
		& Docker.volume "/var/www:/var/www"
		& Apt.serviceInstalledRunning "apache2"

	-- My own openid provider. Uses php, so containerized for security
	-- and administrative sanity.
	, standardStableContainer "openid-provider"
		& alias "openid.kitenet.net"
		& Docker.publish "8081:80"
		& OpenId.providerFor ["joey", "liw"]
			"openid.kitenet.net:8081"

	-- Exhibit: kite's 90's website.
	, standardStableContainer "ancient-kitenet"
		& alias "ancient.kitenet.net"
		& Docker.publish "1994:80"
		& Apt.serviceInstalledRunning "apache2"
		& Git.cloned "root" "git://kitenet-net.branchable.com/" "/var/www"
			(Just "remotes/origin/old-kitenet.net")
	
	, standardStableContainer "oldusenet-shellbox"
		& alias "shell.olduse.net"
		& Docker.publish "4200:4200"
		& JoeySites.oldUseNetShellBox

	-- git-annex autobuilder containers
	, GitAnnexBuilder.standardAutoBuilderContainer dockerImage "amd64" 15 "2h"
	, GitAnnexBuilder.standardAutoBuilderContainer dockerImage "i386" 45 "2h"
	, GitAnnexBuilder.armelCompanionContainer dockerImage
	, GitAnnexBuilder.armelAutoBuilderContainer dockerImage "1 3 * * *" "5h"
	, GitAnnexBuilder.androidAutoBuilderContainer dockerImage "1 1 * * *" "3h"

	-- for development of git-annex for android, using my git-annex
	-- work tree
	, let gitannexdir = GitAnnexBuilder.homedir </> "git-annex"
	  in GitAnnexBuilder.androidContainer dockerImage "android-git-annex" doNothing gitannexdir
		& Docker.volume ("/home/joey/src/git-annex:" ++ gitannexdir)
	]

type Motd = [String]

-- This is my standard system setup.
standardSystem :: HostName -> DebianSuite -> Architecture -> Motd -> Host
standardSystem hn suite arch motd = standardSystemUnhardened hn suite arch motd
	-- Harden the system, but only once root's authorized_keys
	-- is safely in place.
	& check (Ssh.hasAuthorizedKeys "root")
		(Ssh.passwordAuthentication False)

standardSystemUnhardened :: HostName -> DebianSuite -> Architecture -> Motd -> Host
standardSystemUnhardened hn suite arch motd = host hn
	& os (System (Debian suite) arch)
	& Hostname.sane
	& Hostname.searchDomain
	& File.hasContent "/etc/motd" ("":motd++[""])
	& Apt.stdSourcesList `onChange` Apt.upgrade
	& Apt.cacheCleaned
	& Apt.installed ["etckeeper"]
	& Apt.installed ["ssh"]
	& GitHome.installedFor "root"
	& User.hasSomePassword "root" (Context hn)
	& User.accountFor "joey"
	& User.hasSomePassword "joey" (Context hn)
	& Sudo.enabledFor "joey"
	& GitHome.installedFor "joey"
	& Apt.installed ["vim", "screen", "less"]
	& Cron.runPropellor "30 * * * *"
	-- I use postfix, or no MTA.
	& Apt.removed ["exim4", "exim4-daemon-light", "exim4-config", "exim4-base"]
		`onChange` Apt.autoRemove

standardStableContainer :: Docker.ContainerName -> Host
standardStableContainer name = standardContainer name (Stable "wheezy") "amd64"

-- This is my standard container setup, featuring automatic upgrades.
standardContainer :: Docker.ContainerName -> DebianSuite -> Architecture -> Host
standardContainer name suite arch = Docker.container name (dockerImage system)
	& os system
	& Apt.stdSourcesList `onChange` Apt.upgrade
	& Apt.unattendedUpgrades
	& Apt.cacheCleaned
	& Docker.tweaked
  where
	system = System (Debian suite) arch

-- Docker images I prefer to use.
dockerImage :: System -> Docker.Image
dockerImage (System (Debian Unstable) arch) = "joeyh/debian-unstable-" ++ arch
dockerImage (System (Debian Testing) arch) = "joeyh/debian-unstable-" ++ arch
dockerImage (System (Debian (Stable _)) arch) = "joeyh/debian-stable-" ++ arch
dockerImage _ = "debian-stable-official" -- does not currently exist!

myDnsSecondary :: Property
myDnsSecondary = propertyList "dns secondary for all my domains" $ map toProp
	[ Dns.secondary hosts "kitenet.net"
	, Dns.secondary hosts "joeyh.name"
	, Dns.secondary hosts "ikiwiki.info"
	, Dns.secondary hosts "olduse.net"
	]

branchableSecondary :: RevertableProperty
branchableSecondary = Dns.secondaryFor ["branchable.com"] hosts "branchable.com"

-- Currently using diatom (ns2) as primary with secondaries
-- elephant (ns3) and gandi.
-- kite handles all mail.
myDnsPrimary :: Domain -> [(BindDomain, Record)] -> RevertableProperty
myDnsPrimary domain extras = Dns.primary hosts domain
	(Dns.mkSOA "ns2.kitenet.net" 100) $
	[ (RootDomain, NS $ AbsDomain "ns2.kitenet.net")
	, (RootDomain, NS $ AbsDomain "ns3.kitenet.net")
	, (RootDomain, NS $ AbsDomain "ns6.gandi.net")
	, (RootDomain, MX 0 $ AbsDomain "kitenet.net")
	, (RootDomain, TXT "v=spf1 a ?all")
	] ++ extras


                          --                                o
                          --             ___                 o              o
                       {-----\          / o \              ___o            o
                       {      \    __   \   /   _        (X___>--         __o
  _____________________{ ______\___  \__/ | \__/ \____                  |X__>
 <                                  \___//|\\___/\     \____________   _
  \                                  ___/ | \___    # #             \ (-)
   \    O      O      O             #     |     \ #                  >=)
    \______________________________# #   /       #__________________/ (-}


monsters :: [Host]    -- Systems I don't manage with propellor,
monsters =            -- but do want to track their public keys etc.
	[ host "usw-s002.rsync.net"
		& sshPubKey "ssh-dss AAAAB3NzaC1kc3MAAAEBAI6ZsoW8a+Zl6NqUf9a4xXSMcV1akJHDEKKBzlI2YZo9gb9YoCf5p9oby8THUSgfh4kse7LJeY7Nb64NR6Y/X7I2/QzbE1HGGl5mMwB6LeUcJ74T3TQAlNEZkGt/MOIVLolJHk049hC09zLpkUDtX8K0t1yaCirC9SxDGLTCLEhvU9+vVdVrdQlKZ9wpLUNbdAzvbra+O/IVvExxDZ9WCHrnfNA8ddVZIGEWMqsoNgiuCxiXpi8qL+noghsSQNFTXwo7W2Vp9zj1JkCt3GtSz5IzEpARQaXEAWNEM0n1nJ686YUOhou64iRM8bPC1lp3QXvvZNgj3m+QHhIempx+de8AAAAVAKB5vUDaZOg14gRn7Bp81ja/ik+RAAABACPH/bPbW912x1NxNiikzGR6clLh+bLpIp8Qie3J7DwOr8oC1QOKjNDK+UgQ7mDQEgr4nGjNKSvpDi4c1QCw4sbLqQgx1y2VhT0SmUPHf5NQFldRQyR/jcevSSwOBxszz3aq9AwHiv9OWaO3XY18suXPouiuPTpIcZwc2BLDNHFnDURQeGEtmgqj6gZLIkTY0iw7q9Tj5FOyl4AkvEJC5B4CSzaWgey93Wqn1Imt7KI8+H9lApMKziVL1q+K7xAuNkGmx5YOSNlE6rKAPtsIPHZGxR7dch0GURv2jhh0NQYvBRn3ukCjuIO5gx56HLgilq59/o50zZ4NcT7iASF76TcAAAEAC6YxX7rrs8pp13W4YGiJHwFvIO1yXLGOdqu66JM0plO4J1ItV1AQcazOXLiliny3p2/W+wXZZKd5HIRt52YafCA8YNyMk/sF7JcTR4d4z9CfKaAxh0UpzKiAk+0j/Wu3iPoTOsyt7N0j1+dIyrFodY2sKKuBMT4TQ0yqQpbC+IDQv2i1IlZAPneYGfd5MIGygs2QMfaMQ1jWAKJvEO0vstZ7GB6nDAcg4in3ZiBHtomx3PL5w+zg48S4Ed69BiFXLZ1f6MnjpUOP75pD4MP6toS0rgK9b93xCrEQLgm4oD/7TCHHBo2xR7wwcsN2OddtwWsEM2QgOkt/jdCAoVCqwQ=="
	, host "github.com" 
		& sshPubKey "ssh-rsa AAAAB3NzaC1yc2EAAAABIwAAAQEAq2A7hRGmdnm9tUDbO9IDSwBK6TbQa+PXYPCPy6rbTrTtw7PHkccKrpp0yVhp5HdEIcKr6pLlVDBfOLX9QUsyCOV0wzfjIJNlGEYsdlLJizHhbn2mUjvSAHQqZETYP81eFzLQNnPHt4EVVUh7VfDESU84KezmD5QlWpXLmvU31/yMf+Se8xhHTvKSCZIFImWwoG6mbUoWf9nzpIoaSjB+weqqUUmpaaasXVal72J+UX2B+2RPW3RcT0eOzQgqlJL3RKrTJvdsjE3JEAvGq3lGHSZXy28G3skua2SmVi/w4yCE6gbODqnTWlg7+wC604ydGXA8VJiS5ap43JXiUFFAaQ=="
	, host "ns6.gandi.net"
		& ipv4 "217.70.177.40"
	, host "turtle.kitenet.net"
		& ipv4 "67.223.19.96"
		& ipv6 "2001:4978:f:2d9::2"
		& alias "backup.kitenet.net"
		& alias "usbackup.kitenet.net"
		& sshPubKey "ssh-rsa AAAAB3NzaC1yc2EAAAABIwAAAQEAokMXQiX/NZjA1UbhMdgAscnS5dsmy+Q7bWrQ6tsTZ/o+6N/T5cbjoBHOdpypXJI3y/PiJTDJaQtXIhLa8gFg/EvxMnMz/KG9skADW1361JmfCc4BxicQIO2IOOe6eilPr+YsnOwiHwL0vpUnuty39cppuMWVD25GzxXlS6KQsLCvXLzxLLuNnGC43UAM0q4UwQxDtAZEK1dH2o3HMWhgMP2qEQupc24dbhpO3ecxh2C9678a3oGDuDuNf7mLp3s7ptj5qF3onitpJ82U5o7VajaHoygMaSRFeWxP2c13eM57j3bLdLwxVXFhePcKXARu1iuFTLS5uUf3hN6MkQcOGw=="
	, host "old.kitenet.net"
		& ipv4 "80.68.85.49"
	, host "mouse.kitenet.net"
		& ipv6 "2001:4830:1600:492::2"
	, host "beaver.kitenet.net"
		& ipv6 "2001:4830:1600:195::2"
	, host "hydra.kitenet.net"
		& ipv4 "192.25.206.60"
	, host "branchable.com"
		& ipv4 "66.228.46.55"
		& ipv6 "2600:3c03::f03c:91ff:fedf:c0e5"
		& alias "olduse.net"
		& alias "www.olduse.net"
		& alias "www.kitenet.net"
		& alias "joeyh.name"
		& alias "campaign.joeyh.name"
		& alias "ikiwiki.info"
		& alias "git.ikiwiki.info"
		& alias "l10n.ikiwiki.info"
		& alias "dist-bugs.kitenet.net"
		& alias "family.kitenet.net"
	, host "animx"
		& ipv4 "76.7.162.101"
		& ipv4 "76.7.162.186"
	]

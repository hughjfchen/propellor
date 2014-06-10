-- This is the live config file used by propellor's author.
module Main where

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
--import qualified Propellor.Property.Reboot as Reboot
import qualified Propellor.Property.Tor as Tor
import qualified Propellor.Property.Dns as Dns
import qualified Propellor.Property.OpenId as OpenId
import qualified Propellor.Property.Docker as Docker
import qualified Propellor.Property.Git as Git
import qualified Propellor.Property.Apache as Apache
import qualified Propellor.Property.Postfix as Postfix
import qualified Propellor.Property.Service as Service
import qualified Propellor.Property.HostingProvider.DigitalOcean as DigitalOcean
import qualified Propellor.Property.HostingProvider.CloudAtCost as CloudAtCost
import qualified Propellor.Property.SiteSpecific.GitHome as GitHome
import qualified Propellor.Property.SiteSpecific.GitAnnexBuilder as GitAnnexBuilder
import qualified Propellor.Property.SiteSpecific.JoeySites as JoeySites


main :: IO ()           --     _         ______`|                       ,-.__ 
main = defaultMain hosts --  /   \___-=O`/|O`/__|                      (____.'
  {- Propellor            -- \          / | /    )          _.-"-._
     Deployed -}          --  `/-==__ _/__|/__=-|          (       \_
hosts :: [Host]          --   *             \ | |           '--------'
hosts =                 --                  (o)  `
	-- My laptop
	[ host "darkstar.kitenet.net"
		& ipv6 "2001:4830:1600:187::2" -- sixxs tunnel

		& Apt.buildDep ["git-annex"] `period` Daily
		& Docker.configured
		& Docker.docked hosts "android-git-annex"

	-- Nothing super-important lives here and mostly it's docker containers.
	, standardSystem "clam.kitenet.net" Unstable "amd64"
		& ipv4 "162.248.143.249"
		& ipv6 "2002:5044:5531::1"

		& CloudAtCost.decruft
		& Apt.unattendedUpgrades
		& Network.ipv6to4
		& Tor.isBridge
		& Postfix.satellite
		& Docker.configured

		& Docker.docked hosts "oldusenet-shellbox"
		& Docker.docked hosts "openid-provider"
		 	`requires` Apt.serviceInstalledRunning "ntp"
		& Docker.docked hosts "ancient-kitenet"

		-- I'd rather this were on diatom, but it needs unstable.
		& alias "kgb.kitenet.net"
		& JoeySites.kgbServer

		& alias "mumble.kitenet.net"
		& JoeySites.mumbleServer hosts
		
		& alias "ns9.kitenet.net"
		& myDnsSecondary
		
		& alias "znc.kitenet.net"
		& JoeySites.ircBouncer

		-- For https port 443, shellinabox with ssh login to
		-- kitenet.net
		& alias "shell.kitenet.net"
		& JoeySites.kiteShellBox

		-- Nothing is using http port 80 on clam, so listen on
		-- that port for ssh, for traveling on bad networks that
		-- block 22.
		& "/etc/ssh/sshd_config" `File.containsLine` "Port 80"
			`onChange` Service.restarted "ssh"

		& Docker.garbageCollected `period` Daily
		& Apt.installed ["git-annex", "mtr", "screen"]
	
	-- Orca is the main git-annex build box.
	, standardSystem "orca.kitenet.net" Unstable "amd64"
		& ipv4 "138.38.108.179"

		& Hostname.sane
		& Apt.unattendedUpgrades
		& Postfix.satellite
		& Docker.configured
		& Docker.docked hosts "amd64-git-annex-builder"
		& Docker.docked hosts "i386-git-annex-builder"
		& Docker.docked hosts "armel-git-annex-builder-companion"
		& Docker.docked hosts "armel-git-annex-builder"
		& Docker.docked hosts "android-git-annex-builder"
		& Docker.garbageCollected `period` Daily
		& Apt.buildDep ["git-annex"] `period` Daily
	
	-- Important stuff that needs not too much memory or CPU.
  	, standardSystem "diatom.kitenet.net" Stable "amd64"
		& ipv4 "107.170.31.195"

		& DigitalOcean.distroKernel
		& Hostname.sane
		& Ssh.hostKey SshDsa
		& Ssh.hostKey SshRsa
		& Ssh.hostKey SshEcdsa
		& Apt.unattendedUpgrades
		& Apt.serviceInstalledRunning "ntp"
		& Postfix.satellite

		-- Diatom has 500 mb of memory, so tune for that.
		& JoeySites.obnamLowMem
		& Apt.serviceInstalledRunning "swapspace"
	
		& Apt.serviceInstalledRunning "apache2"
		& File.hasPrivContent "/etc/ssl/certs/web.pem"
		& File.hasPrivContent "/etc/ssl/private/web.pem"
		& File.hasPrivContent "/etc/ssl/certs/startssl.pem"
		& Apache.modEnabled "ssl"
		& Apache.multiSSL
		& File.ownerGroup "/srv/web" "joey" "joey"
		& Apt.installed ["analog"]

		& alias "git.kitenet.net"
		& alias "git.joeyh.name"
		& JoeySites.gitServer hosts
	
		& alias "downloads.kitenet.net"
		& JoeySites.annexWebSite hosts "/srv/git/downloads.git"
			"downloads.kitenet.net"
			"840760dc-08f0-11e2-8c61-576b7e66acfd"
			[("turtle", "ssh://turtle.kitenet.net/~/lib/downloads/")]
		& JoeySites.gitAnnexDistributor

		& alias "tmp.kitenet.net"
		& JoeySites.annexWebSite hosts "/srv/git/joey/tmp.git"
			"tmp.kitenet.net"
			"26fd6e38-1226-11e2-a75f-ff007033bdba"
			[]
		& JoeySites.twitRss
		
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

	-- storage and backup server
	, standardSystem "elephant.kitenet.net" Unstable "amd64"
		& ipv4 "193.234.225.114"

		& Hostname.sane
		& Postfix.satellite
		& Apt.unattendedUpgrades
		& Ssh.hostKey SshDsa
		& Ssh.hostKey SshRsa
		& Ssh.hostKey SshEcdsa
		& Ssh.keyImported SshRsa "joey"

		-- PV-grub chaining
		-- http://notes.pault.ag/linode-pv-grub-chainning/
		-- (Adapted to use xvda1/hd0,0 instead of xvda/hd0)
		& "/boot/grub/menu.lst" `File.hasContent`
			[ "default 1" 
			, "timeout 30"
			, ""
			, "title grub-xen shim"
			, "root (hd0,0)"
			, "kernel /boot/xen-shim"
			, "boot"
			]
		& "/boot/load.cf" `File.hasContent`
			[ "configfile (xen/xvda1)/boot/grub/grub.cfg" ]
		& Apt.installed ["grub-xen"]
		& flagFile (scriptProperty ["update-grub; grub-mkimage --prefix '(xen/xvda1)/boot/grub' -c /boot/load.cf -O x86_64-xen /usr/lib/grub/x86_64-xen/*.mod > /boot/xen-shim"]) "/boot/xen-shim"

		& alias "eubackup.kitenet.net"
		& Apt.installed ["obnam", "sshfs", "rsync"]
		& JoeySites.githubBackup
		& JoeySites.obnamRepos ["wren", "pell"]
		& Ssh.knownHost hosts "usw-s002.rsync.net" "joey"

		& alias "podcatcher.kitenet.net"
		& Apt.installed ["git-annex"]
		
		& Docker.configured
		& Docker.garbageCollected `period` (Weekly (Just 1))

	    --'                        __|II|      ,.
	  ----                      __|II|II|__   (  \_,/\
	 ------'\o/-'-.-'-.-'-.- __|II|II|II|II|___/   __/ -'-.-'-.-'-.-'-.-'-
	----------------------- |      [Docker]       / ----------------------
	----------------------- :                    / -----------------------
	------------------------ \____, o          ,' ------------------------
	------------------------- '--,___________,'  -------------------------

	-- Simple web server, publishing the outside host's /var/www
	, standardContainer "webserver" Stable "amd64"
		& Docker.publish "8080:80"
		& Docker.volume "/var/www:/var/www"
		& Apt.serviceInstalledRunning "apache2"

	-- My own openid provider. Uses php, so containerized for security
	-- and administrative sanity.
	, standardContainer "openid-provider" Stable "amd64"
		& alias "openid.kitenet.net"
		& Docker.publish "8081:80"
		& OpenId.providerFor ["joey", "liw"]
			"openid.kitenet.net:8081"

	-- Exhibit: kite's 90's website.
	, standardContainer "ancient-kitenet" Stable "amd64"
		& alias "ancient.kitenet.net"
		& Docker.publish "1994:80"
		& Apt.serviceInstalledRunning "apache2"
		& Git.cloned "root" "git://kitenet-net.branchable.com/" "/var/www"
			(Just "remotes/origin/old-kitenet.net")
	
	, standardContainer "oldusenet-shellbox" Stable "amd64"
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

	-- temp for an acquantance
	] ++ monsters

-- This is my standard system setup.
standardSystem :: HostName -> DebianSuite -> Architecture -> Host
standardSystem hn suite arch = host hn
	& os (System (Debian suite) arch)
	& Apt.stdSourcesList `onChange` Apt.upgrade
	& Apt.cacheCleaned
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

-- This is my standard container setup, featuring automatic upgrades.
standardContainer :: Docker.ContainerName -> DebianSuite -> Architecture -> Host
standardContainer name suite arch = Docker.container name (dockerImage system)
	& os system
	& Apt.stdSourcesList `onChange` Apt.upgrade
	& Apt.installed ["systemd"]
	& Apt.unattendedUpgrades
	& Apt.cacheCleaned
  where
	system = System (Debian suite) arch

-- Docker images I prefer to use.
dockerImage :: System -> Docker.Image
dockerImage (System (Debian Unstable) arch) = "joeyh/debian-unstable-" ++ arch
dockerImage (System (Debian Testing) arch) = "joeyh/debian-unstable-" ++ arch
dockerImage (System (Debian Stable) arch) = "joeyh/debian-stable-" ++ arch
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
-- clam (ns9) and gandi.
-- kite handles all mail.
myDnsPrimary :: Domain -> [(BindDomain, Record)] -> RevertableProperty
myDnsPrimary domain extras = Dns.primary hosts domain
	(Dns.mkSOA "ns2.kitenet.net" 100) $
	[ (RootDomain, NS $ AbsDomain "ns2.kitenet.net")
	, (RootDomain, NS $ AbsDomain "ns6.gandi.net")
	, (RootDomain, NS $ AbsDomain "ns9.kitenet.net")
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
monsters =	      -- but do want to track their public keys etc.
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
		& sshPubKey "ssh-rsa AAAAB3NzaC1yc2EAAAABIwAAAQEAokMXQiX/NZjA1UbhMdgAscnS5dsmy+Q7bWrQ6tsTZ/o+6N/T5cbjoBHOdpypXJI3y/PiJTDJaQtXIhLa8gFg/EvxMnMz/KG9skADW1361JmfCc4BxicQIO2IOOe6eilPr+YsnOwiHwL0vpUnuty39cppuMWVD25GzxXlS6KQsLCvXLzxLLuNnGC43UAM0q4UwQxDtAZEK1dH2o3HMWhgMP2qEQupc24dbhpO3ecxh2C9678a3oGDuDuNf7mLp3s7ptj5qF3onitpJ82U5o7VajaHoygMaSRFeWxP2c13eM57j3bLdLwxVXFhePcKXARu1iuFTLS5uUf3hN6MkQcOGw=="
	, host "wren.kitenet.net"
		& ipv4 "80.68.85.49"
		& ipv6 "2001:41c8:125:49::10"
		& alias "kitenet.net"
		& alias "kite.kitenet.net"
		& alias "ns1.kitenet.net"
		& alias "ftp.kitenet.net"
		& alias "mail.kitenet.net"
		& alias "smtp.kitenet.net"
		& alias "sows-ear.kitenet.net"
		& alias "www.sows-ear.kitenet.net"
		& alias "wortroot.kitenet.net"
		& alias "www.wortroot.kitenet.net"
		& alias "joey.kitenet.net"
		& alias "anna.kitenet.net"
		& alias "bitlbee.kitenet.net"
		{- Remaining services on kite:
		 - 
		 - mail
		 -   postfix
		 -   postgrey
		 -   mailman
		 -   spamassassin
		 -   sqwebmail
		 -   courier
		 -     imap
		 -     tls
		 - apache
		 -   some static websites
		 - bitlbee
		 - prosody
		 -   (used by anna and daddy's git-annex)
		 - named
		 -   (branchable is still pushing to here
		 -    (thinking it's ns2.branchable.com), but it's no
		 -   longer a primary or secondary for anything)
		 - ftpd (EOL)
		 -
		 - user shell stuff:
		 -   pine, zsh, make, git-annex, myrepos, ...
		 -}
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

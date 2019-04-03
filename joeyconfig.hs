-- This is the live config file used by propellor's author.
-- https://propellor.branchable.com/
module Main where

import Propellor
import Propellor.Property.Scheduled
import Propellor.Property.DiskImage
import Propellor.Property.Chroot
import Propellor.Property.Machine
import Propellor.Property.Bootstrap
import qualified Propellor.Property.File as File
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.Network as Network
import qualified Propellor.Property.Ssh as Ssh
import qualified Propellor.Property.Cron as Cron
import qualified Propellor.Property.Sudo as Sudo
import qualified Propellor.Property.User as User
import qualified Propellor.Property.Hostname as Hostname
import qualified Propellor.Property.Fstab as Fstab
import qualified Propellor.Property.Tor as Tor
import qualified Propellor.Property.Dns as Dns
import qualified Propellor.Property.Git as Git
import qualified Propellor.Property.Postfix as Postfix
import qualified Propellor.Property.Apache as Apache
import qualified Propellor.Property.LetsEncrypt as LetsEncrypt
import qualified Propellor.Property.Locale as Locale
import qualified Propellor.Property.Grub as Grub
import qualified Propellor.Property.Borg as Borg
import qualified Propellor.Property.Gpg as Gpg
import qualified Propellor.Property.OpenId as OpenId
import qualified Propellor.Property.Systemd as Systemd
import qualified Propellor.Property.Journald as Journald
import qualified Propellor.Property.Fail2Ban as Fail2Ban
import qualified Propellor.Property.Laptop as Laptop
import qualified Propellor.Property.HostingProvider.Linode as Linode
import qualified Propellor.Property.HostingProvider.DigitalOcean as DigitalOcean
import qualified Propellor.Property.SiteSpecific.GitHome as GitHome
import qualified Propellor.Property.SiteSpecific.GitAnnexBuilder as GitAnnexBuilder
import qualified Propellor.Property.SiteSpecific.Branchable as Branchable
import qualified Propellor.Property.SiteSpecific.JoeySites as JoeySites

main :: IO ()           --     _         ______`|                       ,-.__
main = defaultMain hosts --  /   \___-=O`/|O`/__|                      (____.'
  {- Propellor            -- \          / | /    )          _.-"-._
     Deployed -}          --  `/-==__ _/__|/__=-|          (       \_
hosts :: [Host]          --   *             \ | |           '--------'
hosts =                 --                  (o)  `
	[ darkstar
	, dragon
	, clam
	, orca
	, baleen
	, honeybee
	, kite
	, beaver
	, sow
	, mouse
	, peregrine
	, pell
	, keysafe
	] ++ monsters

darkstar :: Host
darkstar = host "darkstar.kitenet.net" $ props
	& osDebian Unstable X86_64
	& ipv6 "2001:4830:1600:187::2"
	& Hostname.sane
	& Hostname.mailname
	& Apt.serviceInstalledRunning "swapspace"
	& Laptop.powertopAutoTuneOnBoot
	& Laptop.trimSSD
	& Grub.cmdline_Linux_default "i915.enable_psr=1"
	! Grub.cmdline_Linux_default "quiet"
	& User.hasGroup (User "joey") (Group "dialout")

	& JoeySites.dkimMilter
	& JoeySites.postfixSaslPasswordClient
	-- & JoeySites.alarmClock "*-*-* 7:30" (User "joey")
	--	"/usr/bin/timeout 45m /home/joey/bin/goodmorning"
	& JoeySites.laptopSoftware
	& JoeySites.userDirHtml
	& Ssh.userKeys (User "joey") hostContext
		[ (SshEd25519, "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICfFntnesZcYz2B2T41ay45igfckXRSh5uVffkuCQkLv joey@darkstar")
		]
	& imageBuiltFor honeybee
		(RawDiskImage "/srv/honeybee.img")
		(Debootstrapped mempty)
	! imageBuiltFor banana
		(RawDiskImage "/srv/banana.img")
		(Debootstrapped mempty)

dragon :: Host
dragon = host "dragon.kitenet.net" $ props
	& ipv6 "2001:4830:1600:187::2"
	& JoeySites.dkimMilter
	& JoeySites.postfixSaslPasswordClient

clam :: Host
clam = host "clam.kitenet.net" $ props
	& standardSystem (Stable "stretch") X86_64
		["Unreliable server. Anything here may be lost at any time!" ]
	& ipv4 "178.33.208.168"

	& User.hasPassword (User "root")
	& Ssh.hostKeys hostContext
		[ (SshDsa, "ssh-dss AAAAB3NzaC1kc3MAAACBAI3WUq0RaigLlcUivgNG4sXpso2ORZkMvfqKz6zkc60L6dpxvWDNmZVEH8hEjxRSYG07NehcuOgQqeyFnS++xw1hdeGjf37JqCUH49i02lra3Zxv8oPpRxyeqe5MmuzUJhlWvBdlc3O/nqZ4bTUfnxMzSYWyy6++s/BpSHttZplNAAAAFQC1DE0vzgVeNAv9smHLObQWZFe2VQAAAIBECtpJry3GC8NVTFsTHDGWksluoFPIbKiZUFFztZGdM0AO2VwAbiJ6Au6M3VddGFANgTlni6d2/9yS919zO90TaFoIjywZeXhxE2CSuRfU7sx2hqDBk73jlycem/ER0sanFhzpHVpwmLfWneTXImWyq37vhAxatJANOtbj81vQ3AAAAIBV3lcyTT9xWg1Q4vERJbvyF8mCliwZmnIPa7ohveKkxlcgUk5d6dnaqFfjVaiXBPN3Qd08WXoQ/a9k3chBPT9nW2vWgzzM8l36j2MbHLmaxGwevAc9+vx4MXqvnGHzd2ex950mC33ct3j0fzMZlO6vqEsgD4CYmiASxhfefj+JCQ==")
		, (SshRsa, "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDJybAjUPUWIhvVMmer8K5ZgdfI54DM6vc8Mzw+5KmVKL0TwkvzbR1HAB4heyMGtN1F8YzkWhsI3/Txh+MQUJ+i4u8SvSYc6D1q3j3ZyCi06wZ3DJS25tZrOM/thOOA1DFA4Hhb0uI/1Kg8PguNNNSMXn8F7q3F6cFQizYgszs6z6ktiST/BTC+IXWovhcnn2vQXXU8FTcTsqBFqA5dEjZbp1WDzqp3km84ZyXGmoVlpqzXeMvlkWTIshYiQjXIwPOkALzlGYjp1lw1OaxPVI1IGFcgCbIWQQWoCReb+genX2VaR+odAYXjaOdRx0lQj7UCPTBCpqMyzBMLtT5Yiaqh")
		, (SshEcdsa, "ecdsa-sha2-nistp256 AAAAE2VjZHNhLXNoYTItbmlzdHAyNTYAAAAIbmlzdHAyNTYAAABBBPhfvcOuw0Yt+MnsFc4TI2gWkKi62Eajxz+TgbHMO/uRTYF8c5V8fOI3o+J/3m5+lT0S5o8j8a7xIC3COvi+AVw=")
		]
	& Apt.unattendedUpgrades
	& Apt.serviceInstalledRunning "swapspace"

	& Tor.isRelay
	& Tor.named "kite1"
	& Tor.bandwidthRate (Tor.PerMonth "400 GB")
	
	& "/etc/resolv.conf" `File.hasContent`
		[ "nameserver 8.8.8.8"
		, "nameserver 8.8.4.4"
		, "nameserver 1.1.1.1"
		, "domain kitenet.net"
		, "search kitenet.net"
		]

baleen :: Host
baleen = host "baleen.kitenet.net" $ props
	& standardSystem Unstable X86_64 [ "New git-annex build box." ]

	-- Not on public network; ssh access via bounce host.
	& ipv4 "138.38.77.40"
	
	-- The root filesystem content may be lost if the VM is resized.
	-- /dev/vdb contains persistent storage.
	& Fstab.mounted "auto" "/dev/vdb" "/var/lib/container" mempty
	
	& Apt.unattendedUpgrades
	& Postfix.satellite
	& Apt.serviceInstalledRunning "ntp"
	& Systemd.persistentJournal

orca :: Host
orca = host "orca.kitenet.net" $ props
	& standardSystem Unstable X86_64 [ "Main git-annex build box." ]
	& ipv4 "138.38.108.179"

	& Apt.unattendedUpgrades
	& Postfix.satellite
	& Apt.serviceInstalledRunning "ntp"
	& Systemd.persistentJournal

	& Systemd.nspawned (GitAnnexBuilder.autoBuilderContainer
		GitAnnexBuilder.standardAutoBuilder
		Unstable X86_64 Nothing (Cron.Times "15 * * * *") "2h")
	& Systemd.nspawned (GitAnnexBuilder.autoBuilderContainer
		GitAnnexBuilder.standardAutoBuilder
		Unstable X86_32 Nothing (Cron.Times "30 * * * *") "2h")
	& Systemd.nspawned (GitAnnexBuilder.autoBuilderContainer
		GitAnnexBuilder.stackAutoBuilder
		(Stable "jessie") X86_32 (Just "ancient") (Cron.Times "45 * * * *") "2h")
	& Systemd.nspawned (GitAnnexBuilder.autoBuilderContainer
		GitAnnexBuilder.standardAutoBuilder
		Testing ARM64 Nothing (Cron.Times "1 * * * *") "4h")

banana :: Host
banana = host "banana.kitenet.net" $ props
	& lemaker_Banana_Pi
	& hasPartition
		( partition EXT4
			`mountedAt` "/"
			`setSize` MegaBytes 950
		)
	& osDebian Testing ARMHF
	& User.hasInsecurePassword (User "root") "root"

honeybee :: Host
honeybee = host "honeybee.kitenet.net" $ props
	& standardSystem Testing ARMHF
		[ "Home router and arm git-annex build box." ]
	& Apt.removed ["rsyslog"]
	
	& cubietech_Cubietruck
	& hasPartition
		( partition EXT3
			`mountedAt` "/"
			`setSize` MegaBytes 16000
		)
	& JoeySites.cubieTruckOneWire
	& Systemd.persistentJournal
	& Apt.installed ["firmware-atheros"]
	& Apt.serviceInstalledRunning "ntp" -- no hardware clock
	& bootstrappedFrom GitRepoOutsideChroot
	& Ssh.hostKeys hostContext
		[ (SshEd25519, "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIIS/hDYq1MAxfOBf49htym3BOYlx4Gk9SDpiHjv7u6IC")
		]

	& JoeySites.homePower
		(User "joey")
		hosts
		(Context "homepower.joeyh.name")
		(SshEd25519, "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMAmVYddg/RgCbIj+cLcEiddeFXaYFnbEJ3uGj9G/EyV joey@honeybee")
	& JoeySites.homeRouter
	& JoeySites.homeNAS
	& Apt.installed ["mtr-tiny", "iftop", "screen"]
	& Postfix.satellite

	& check (not <$> inChroot) (setupRevertableProperty autobuilder)
	-- In case compiler needs more than available ram
	& Apt.serviceInstalledRunning "swapspace"
  where
	autobuilder = Systemd.nspawned $ GitAnnexBuilder.autoBuilderContainer
		(GitAnnexBuilder.armAutoBuilder GitAnnexBuilder.standardAutoBuilder)
		Testing ARMEL Nothing (Cron.Times "15 15 * * *") "10h"

-- This is not a complete description of kite, since it's a
-- multiuser system with eg, user passwords that are not deployed
-- with propellor.
kite :: Host
kite = host "kite.kitenet.net" $ props
	& standardSystemUnhardened Testing X86_64 [ "Welcome to kite!" ]
	& ipv4 "66.228.36.95"
	& ipv6 "2600:3c03::f03c:91ff:fe73:b0d2"
	& alias "kitenet.net"
	& Ssh.hostKeys (Context "kitenet.net")
		[ (SshDsa, "ssh-dss AAAAB3NzaC1kc3MAAACBAO9tnPUT4p+9z7K6/OYuiBNHaij4Nzv5YVBih1vMl+ALz0gYAj8RWJzXmqp5buFAyfgOoLw+H9s1bBS01Sy3i07Dm6cx1fWG4RXL/E/3w1tavX99GD2bBxDBu890ebA5Tp+eFRJkS9+JwSvFiF6CP7NbVjifCagoUO56Ig048RwDAAAAFQDPY2xM3q6KwsVQliel23nrd0rV2QAAAIEAga3hj1hL00rYPNnAUzT8GAaSP62S4W68lusErH+KPbsMwFBFY/Ib1FVf8k6Zn6dZLh/HH/RtJi0JwdzPI1IFW+lwVbKfwBvhQ1lw9cH2rs1UIVgi7Wxdgfy8gEWxf+QIqn62wG+Ulf/HkWGvTrRpoJqlYRNS/gnOWj9Z/4s99koAAACBAM/uJIo2I0nK15wXiTYs/NYUZA7wcErugFn70TRbSgduIFH6U/CQa3rgHJw9DCPCQJLq7pwCnFH7too/qaK+czDk04PsgqV0+Jc7957gU5miPg50d60eJMctHV4eQ1FpwmGGfXxRBR9k2ZvikWYatYir3L6/x1ir7M0bA9IzNU45")
		, (SshRsa, "ssh-rsa AAAAB3NzaC1yc2EAAAABIwAAAIEA2QAJEuvbTmaN9ex9i9bjPhMGj+PHUYq2keIiaIImJ+8mo+yKSaGUxebG4tpuDPx6KZjdycyJt74IXfn1voGUrfzwaEY9NkqOP3v6OWTC3QeUGqDCeJ2ipslbEd9Ep9XBp+/ldDQm60D0XsIZdmDeN6MrHSbKF4fXv1bqpUoUILk=")
		, (SshEcdsa, "ecdsa-sha2-nistp256 AAAAE2VjZHNhLXNoYTItbmlzdHAyNTYAAAAIbmlzdHAyNTYAAABBBLF+dzqBJZix+CWUkAd3Bd3cofFCKwHMNRIfwx1G7dL4XFe6fMKxmrNetQcodo2edyufwoPmCPr3NmnwON9vyh0=")
		, (SshEd25519, "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFZftKMnH/zH29BHMKbcBO4QsgTrstYFVhbrzrlRzBO3")
		]

	& Network.preserveStatic "eth0" `requires` Network.cleanInterfacesFile
	& Apt.installed ["linux-image-amd64"]
	& Apt.serviceInstalledRunning "swapspace"
	& Linode.serialGrub
	& Linode.mlocateEnabled
	& Apt.unattendedUpgrades
	& Systemd.installed
	& Systemd.persistentJournal
	& Journald.systemMaxUse "500MiB"
	& Ssh.passwordAuthentication True
	& Fail2Ban.installed -- since ssh password authentication is allowed
	-- Allow ssh -R to forward ports via kite
	& Ssh.setSshdConfig "GatewayPorts" "clientspecified"
	& Apt.serviceInstalledRunning "ntp"
	& "/etc/timezone" `File.hasContent` ["US/Eastern"]
	
	& Borg.backup "/" (JoeySites.rsyncNetBorgRepo "kite.borg" []) Cron.Daily
		[ "--exclude=/proc/*"
		, "--exclude=/sys/*"
		, "--exclude=/run/*"
		, "--exclude=/mnt/*"
		, "--exclude=/tmp/*"
		, "--exclude=/var/tmp/*"
		, "--exclude=/var/cache/*"
		, "--exclude=/var/lib/swapspace/*"
		, "--exclude=/var/lib/container/*"
		, "--exclude=/home/joey/lib"
		-- These directories are backed up and restored separately.
		, "--exclude=/srv/git"
		, "--exclude=/var/spool/oldusenet"
		]
		[ Borg.KeepDays 7
		, Borg.KeepWeeks 4
		, Borg.KeepMonths 3
		]
		`requires` Ssh.knownHost hosts "usw-s002.rsync.net" (User "root")
		`requires` Ssh.userKeys (User "root")
			(Context "kite.kitenet.net")
			[ (SshRsa, "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQC5Gza2sNqSKfNtUN4dN/Z3rlqw18nijmXFx6df2GtBoZbkIak73uQfDuZLP+AXlyfHocwdkdHEf/zrxgXS4EokQMGLZhJ37Pr3edrEn/NEnqroiffw7kyd7EqaziA6UOezcLTjWGv+Zqg9JhitYs4WWTpNzrPH3yQf1V9FunZnkzb4gJGndts13wGmPEwSuf+QHbgQvjMOMCJwWSNcJGdhDR66hFlxfG26xx50uIczXYAbgLfHp5W6WuR/lcaS9J6i7HAPwcsPDA04XDinrcpl29QwsMW1HyGS/4FSCgrDqNZ2jzP49Bka78iCLRqfl1efyYas/Zo1jQ0x+pxq2RMr root@kite")
			]

	& alias "smtp.kitenet.net"
	& alias "imap.kitenet.net"
	& alias "pop.kitenet.net"
	& alias "mail.kitenet.net"
	& JoeySites.kiteMailServer

	& JoeySites.legacyWebSites
	& File.ownerGroup "/srv/web" (User "joey") (Group "joey")
	& Apt.installed ["analog"]

	& alias "git.kitenet.net"
	& alias "git.joeyh.name"
	& JoeySites.gitServer hosts

	& JoeySites.downloads
	& JoeySites.gitAnnexDistributor
	& JoeySites.tmp

	& Apt.installed
		[ "git-annex", "myrepos"
		, "build-essential", "make"
		, "rss2email", "archivemail"
		, "devscripts"
		-- Some users have zsh as their login shell.
		, "zsh"
		]

	& alias "nntp.olduse.net"
	& JoeySites.oldUseNetServer hosts
	& Systemd.nspawned oldusenetShellBox
	
	& alias "znc.kitenet.net"
	& JoeySites.ircBouncer

	& alias "kgb.kitenet.net"
	& JoeySites.kgbServer
	
	& Systemd.nspawned ancientKitenet
	& Systemd.nspawned openidProvider
	
	& alias "podcatcher.kitenet.net"
	& JoeySites.podcatcher

	& JoeySites.scrollBox
	& alias "scroll.joeyh.name"
	& alias "us.scroll.joeyh.name"

	& alias "ns4.kitenet.net"
	& myDnsPrimary "kitenet.net"
		[ (RelDomain "mouse-onion", CNAME $ AbsDomain "htieo6yu2qtcn2j3.onion")
		, (RelDomain "beaver-onion", CNAME $ AbsDomain "tl4xsvaxryjylgxs.onion")
		, (RelDomain "peregrine-onion", CNAME $ AbsDomain "ahw47zqw6qszoufl.onion")
		, (RelDomain "sow-onion", CNAME $ AbsDomain "urt4g2tq32qktgtp.onion")
		]
	& myDnsPrimary "joeyh.name" []
	& myDnsPrimary "ikiwiki.info" []
	& myDnsPrimary "olduse.net"
		[ (RelDomain "article", CNAME $ AbsDomain "virgil.koldfront.dk")
		]
	& alias "ns4.branchable.com"
	& branchableSecondary
	& Dns.secondaryFor ["animx"] hosts "animx.eu.org"
	-- Use its own name server (amoung other things this avoids
	-- spamassassin URIBL_BLOCKED.
	& "/etc/resolv.conf" `File.hasContent`
		[ "nameserver 127.0.0.1"
		, "domain kitenet.net"
		, "search kitenet.net"
		]
	
	& alias "debug-me.joeyh.name"
	& Apt.installed ["debug-me"]
	& Systemd.enabled "debug-me"

	-- testing
	& Apache.httpsVirtualHost "letsencrypt.joeyh.name" "/var/www/html"
		(LetsEncrypt.AgreeTOS (Just "id@joeyh.name"))
	& alias "letsencrypt.joeyh.name"

beaver :: Host
beaver = host "beaver.kitenet.net" $ props
	& Apt.installed ["ssh"]
	& Ssh.hostPubKey SshDsa "ssh-dss AAAAB3NzaC1kc3MAAACBAIrLX260fY0Jjj/p0syNhX8OyR8hcr6feDPGOj87bMad0k/w/taDSOzpXe0Wet7rvUTbxUjH+Q5wPd4R9zkaSDiR/tCb45OdG6JsaIkmqncwe8yrU+pqSRCxttwbcFe+UU+4AAcinjVedZjVRDj2rRaFPc9BXkPt7ffk8GwEJ31/AAAAFQCG/gOjObsr86vvldUZHCteaJttNQAAAIB5nomvcqOk/TD07DLaWKyG7gAcW5WnfY3WtnvLRAFk09aq1EuiJ6Yba99Zkb+bsxXv89FWjWDg/Z3Psa22JMyi0HEDVsOevy/1sEQ96AGH5ijLzFInfXAM7gaJKXASD7hPbVdjySbgRCdwu0dzmQWHtH+8i1CMVmA2/a5Y/wtlJAAAAIAUZj2US2D378jBwyX1Py7e4sJfea3WSGYZjn4DLlsLGsB88POuh32aOChd1yzF6r6C2sdoPBHQcWBgNGXcx4gF0B5UmyVHg3lIX2NVSG1ZmfuLNJs9iKNu4cHXUmqBbwFYQJBvB69EEtrOw4jSbiTKwHFmqdA/mw1VsMB+khUaVw=="
	& Tor.installed
	& Tor.hiddenServiceAvailable "ssh" (Port 22)

sow :: Host
sow = host "sow.kitenet.net" $ props
	& Apt.installed ["ssh"]
	& Ssh.hostPubKey SshDsa "ssh-dss AAAAB3NzaC1kc3MAAACBAIrLX260fY0Jjj/p0syNhX8OyR8hcr6feDPGOj87bMad0k/w/taDSOzpXe0Wet7rvUTbxUjH+Q5wPd4R9zkaSDiR/tCb45OdG6JsaIkmqncwe8yrU+pqSRCxttwbcFe+UU+4AAcinjVedZjVRDj2rRaFPc9BXkPt7ffk8GwEJ31/AAAAFQCG/gOjObsr86vvldUZHCteaJttNQAAAIB5nomvcqOk/TD07DLaWKyG7gAcW5WnfY3WtnvLRAFk09aq1EuiJ6Yba99Zkb+bsxXv89FWjWDg/Z3Psa22JMyi0HEDVsOevy/1sEQ96AGH5ijLzFInfXAM7gaJKXASD7hPbVdjySbgRCdwu0dzmQWHtH+8i1CMVmA2/a5Y/wtlJAAAAIAUZj2US2D378jBwyX1Py7e4sJfea3WSGYZjn4DLlsLGsB88POuh32aOChd1yzF6r6C2sdoPBHQcWBgNGXcx4gF0B5UmyVHg3lIX2NVSG1ZmfuLNJs9iKNu4cHXUmqBbwFYQJBvB69EEtrOw4jSbiTKwHFmqdA/mw1VsMB+khUaVw=="
	& Tor.installed
	& Tor.hiddenServiceAvailable "ssh" (Port 22)

mouse :: Host
mouse = host "mouse.kitenet.net" $ props
	& ipv4 "67.223.19.96"
	& Apt.installed ["ssh"]
	& Tor.installed
	& Tor.hiddenServiceAvailable "ssh" (Port 22)

peregrine :: Host
peregrine = host "peregrine.kitenet.net" $ props
	& Apt.installed ["ssh"]
	& Tor.installed
	& Tor.hiddenServiceAvailable "ssh" (Port 22)

-- Branchable is not completely deployed with propellor yet.
pell :: Host
pell = host "pell.branchable.com" $ props
	& alias "branchable.com"
	& ipv4 "66.228.46.55"
	& ipv6 "2600:3c03::f03c:91ff:fedf:c0e5"

	-- All the websites I host at branchable that don't use
	-- branchable.com dns.
	& alias "olduse.net"
	& alias "www.olduse.net"
	& alias "www.kitenet.net"
	& alias "joeyh.name"
	& alias "www.joeyh.name"
	& alias "campaign.joeyh.name"
	& alias "ikiwiki.info"
	& alias "www.ikiwiki.info"
	& alias "git.ikiwiki.info"
	& alias "l10n.ikiwiki.info"
	& alias "dist-bugs.kitenet.net"
	& alias "family.kitenet.net"

	& osDebian (Stable "stretch") X86_64
	& Apt.installed ["linux-image-686-pae"]
	& Apt.unattendedUpgrades
	& Branchable.server hosts
	& Linode.serialGrub

-- See https://joeyh.name/code/keysafe/servers/ for requirements.
keysafe :: Host
keysafe = host "keysafe.joeyh.name" $ props
	& ipv4 "139.59.17.168"
	& Hostname.sane
	& Hostname.mailname
	& osDebian (Stable "stretch") X86_64
	& Apt.stdSourcesList `onChange` Apt.upgrade
	& Apt.unattendedUpgrades
	& DigitalOcean.distroKernel
	-- This is a 500 mb VM, so need more ram to build propellor.
	& Apt.serviceInstalledRunning "swapspace"
	& Cron.runPropellor (Cron.Times "30 * * * *")
	& Apt.installed ["etckeeper", "sudo"]
	& Apt.removed ["nfs-common", "exim4", "exim4-base", "exim4-daemon-light", "rsyslog", "acpid", "rpcbind", "at"]

	& User.hasSomePassword (User "root")
	& User.accountFor (User "joey")
	& User.hasSomePassword (User "joey")
	& Sudo.enabledFor (User "joey")

	& Ssh.installed
	& Ssh.randomHostKeys
	& User "root" `Ssh.authorizedKeysFrom` (User "joey", darkstar)
	& User "joey" `Ssh.authorizedKeysFrom` (User "joey", darkstar)
	& Ssh.noPasswords

	& Tor.installed
	& Tor.hiddenServiceAvailable "keysafe" (Port 4242)
		`requires` Tor.hiddenServiceData "keysafe" hostContext
	& Tor.bandwidthRate (Tor.PerMonth "750 GB")

	-- keysafe installed manually until package is available
	& Systemd.enabled "keysafe"

	& Gpg.keyImported (Gpg.GpgKeyId "CECE11AE") (User "root")
	& Ssh.knownHost hosts "usw-s002.rsync.net" (User "root")
	& Ssh.userKeys (User "root")
		(Context "keysafe.joeyh.name")
		[ (SshEd25519, "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIEx8bK9ZbXVEgEvxQeXLjnr9cGa/QvoB459aglP529My root@keysafe")
		]
	-- Note that this is not an incremental backup; it uploads the
	-- whole content every time. So, only run weekly.
	& Cron.niceJob "keysafe backup" Cron.Weekly (User "root") "/" backupcmd
		`requires` Apt.installed ["rsync"]
  where
	datadir = "/var/lib/keysafe"
	backupdir = "/var/backups/keysafe"
	rsyncnetbackup = "2318@usw-s002.rsync.net:keysafe"
	backupcmd = unwords
		[ "keysafe --store-directory", datadir, "--backup-server", backupdir
		, "&& rsync -a --delete --max-delete 3 ",  backupdir , rsyncnetbackup
		]

       --'                        __|II|      ,.
     ----                      __|II|II|__   (  \_,/\
--'-------'\o/-'-.-'-.-'-.- __|II|II|II|II|___/   __/ -'-.-'-.-'-.-'-.-'-.-'-
-------------------------- |   [Containers]      / --------------------------
-------------------------- :                    / ---------------------------
--------------------------- \____, o          ,' ----------------------------
---------------------------- '--,___________,'  -----------------------------

-- Exhibit: kite's 90's website on port 1994.
ancientKitenet :: Systemd.Container
ancientKitenet = Systemd.debContainer "ancient-kitenet" $ props
	& standardContainer (Stable "stretch")
	& alias hn
	& Git.cloned (User "root") "git://kitenet-net.branchable.com/" "/var/www/html"
		(Just "remotes/origin/old-kitenet.net")
	& Apache.installed
	& Apache.listenPorts [p]
	& Apache.virtualHost hn p "/var/www/html"
	& Apache.siteDisabled "000-default"
  where
	p = Port 1994
	hn = "ancient.kitenet.net"

oldusenetShellBox :: Systemd.Container
oldusenetShellBox = Systemd.debContainer "oldusenet-shellbox" $ props
	& standardContainer (Stable "stretch")
	& alias "shell.olduse.net"
	& JoeySites.oldUseNetShellBox

-- My own openid provider. Uses php, so containerized for security
-- and administrative sanity.
openidProvider :: Systemd.Container
openidProvider = Systemd.debContainer "openid-provider" $ props
	& standardContainer (Stable "stretch")
	& alias hn
	& OpenId.providerFor [User "joey", User "liw"] hn (Just (Port 8086))
  where
	hn = "openid.kitenet.net"

type Motd = [String]

-- This is my standard system setup.
standardSystem :: DebianSuite -> Architecture -> Motd -> Property (HasInfo + Debian)
standardSystem suite arch motd =
	standardSystemUnhardened suite arch motd
		`before` Ssh.noPasswords

standardSystemUnhardened :: DebianSuite -> Architecture -> Motd -> Property (HasInfo + Debian)
standardSystemUnhardened suite arch motd = propertyList "standard system" $ props
	& osDebian suite arch
	& Hostname.sane
	& Hostname.mailname
	& Hostname.searchDomain
	& Locale.available "en_US.UTF-8"
	& File.hasContent "/etc/motd" ("":motd++[""])
	& Apt.stdSourcesList `onChange` Apt.upgrade
	& Apt.cacheCleaned
	& Apt.installed ["etckeeper"]
	& Apt.installed ["ssh", "mosh"]
	& GitHome.installedFor (User "root")
	& User.hasSomePassword (User "root")
	& User.accountFor (User "joey")
	& User.hasSomePassword (User "joey")
	& Sudo.enabledFor (User "joey")
	& GitHome.installedFor (User "joey")
	& Apt.installed ["vim", "screen", "less"]
	& Cron.runPropellor (Cron.Times "30 * * * *")
	-- I use postfix, or no MTA.
	& Apt.removed ["exim4", "exim4-daemon-light", "exim4-config", "exim4-base"]
		`onChange` Apt.autoRemove

-- This is my standard container setup, Featuring automatic upgrades.
standardContainer :: DebianSuite -> Property (HasInfo + Debian)
standardContainer suite = propertyList "standard container" $ props
	& osDebian suite X86_64
	& Apt.stdSourcesList `onChange` Apt.upgrade
	& Apt.unattendedUpgrades
	& Apt.cacheCleaned

myDnsSecondary :: Property (HasInfo + DebianLike)
myDnsSecondary = propertyList "dns secondary for all my domains" $ props
	& Dns.secondary hosts "kitenet.net"
	& Dns.secondary hosts "joeyh.name"
	& Dns.secondary hosts "ikiwiki.info"
	& Dns.secondary hosts "olduse.net"

branchableSecondary :: RevertableProperty (HasInfo + DebianLike) DebianLike
branchableSecondary = Dns.secondaryFor ["branchable.com"] hosts "branchable.com"

-- Currently using kite (ns4) as primary with gandi as secondary
-- kite handles all mail.
myDnsPrimary :: Domain -> [(BindDomain, Record)] -> RevertableProperty (HasInfo + DebianLike) DebianLike
myDnsPrimary domain extras = Dns.signedPrimary (Weekly Nothing) hosts domain
	(Dns.mkSOA "ns4.kitenet.net" 100) $
	[ (RootDomain, NS $ AbsDomain "ns4.kitenet.net")
	, (RootDomain, NS $ AbsDomain "ns6.gandi.net")
	, (RootDomain, MX 0 $ AbsDomain "kitenet.net")
	, (RootDomain, TXT "v=spf1 a a:kitenet.net ~all")
	, JoeySites.domainKey
	] ++ extras

-- Systems I don't manage with propellor,
-- but do want to track their public keys etc.
monsters :: [Host]
monsters =
	[ host "usw-s002.rsync.net" $ props
		& Ssh.hostPubKey SshEd25519 "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIB7yTEBGfQYdwG/oeL+U9XPMIh/dW7XNs9T+M79YIOrd"
	, host "github.com" $ props
		& Ssh.hostPubKey SshRsa "ssh-rsa AAAAB3NzaC1yc2EAAAABIwAAAQEAq2A7hRGmdnm9tUDbO9IDSwBK6TbQa+PXYPCPy6rbTrTtw7PHkccKrpp0yVhp5HdEIcKr6pLlVDBfOLX9QUsyCOV0wzfjIJNlGEYsdlLJizHhbn2mUjvSAHQqZETYP81eFzLQNnPHt4EVVUh7VfDESU84KezmD5QlWpXLmvU31/yMf+Se8xhHTvKSCZIFImWwoG6mbUoWf9nzpIoaSjB+weqqUUmpaaasXVal72J+UX2B+2RPW3RcT0eOzQgqlJL3RKrTJvdsjE3JEAvGq3lGHSZXy28G3skua2SmVi/w4yCE6gbODqnTWlg7+wC604ydGXA8VJiS5ap43JXiUFFAaQ=="
	, host "gitlab.com" $ props
		& Ssh.hostPubKey SshEcdsa "ecdsa-sha2-nistp256 AAAAE2VjZHNhLXNoYTItbmlzdHAyNTYAAAAIbmlzdHAyNTYAAABBBFSMqzJeV9rUzU4kWitGjeR4PWSa29SPqJ1fVkhtj3Hw9xjLVXVYrU9QlYWrOLXBpQ6KWjbjTDTdDkoohFzgbEY="
	, host "ns6.gandi.net" $ props
		& ipv4 "217.70.177.40"
	, host "animx" $ props
		& ipv4 "76.7.174.49"
	]



                          --                                o
                          --             ___                 o              o
                       {-----\          / o \              ___o            o
                       {      \    __   \   /   _        (X___>--         __o
  _____________________{ ______\___  \__/ | \__/ \____                  |X__>
 <                                  \___//|\\___/\     \____________   _
  \                                  ___/ | \___    # #             \ (-)
   \    O      O      O             #     |     \ #                  >=)
    \______________________________# #   /       #__________________/ (-}



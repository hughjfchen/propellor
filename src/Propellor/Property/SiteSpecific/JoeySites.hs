-- | Specific configuration for Joey Hess's sites. Probably not useful to
-- others except as an example.

{-# LANGUAGE FlexibleContexts, TypeFamilies #-}

module Propellor.Property.SiteSpecific.JoeySites where

import Propellor.Base
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.File as File
import qualified Propellor.Property.ConfFile as ConfFile
import qualified Propellor.Property.Gpg as Gpg
import qualified Propellor.Property.Ssh as Ssh
import qualified Propellor.Property.Git as Git
import qualified Propellor.Property.Cron as Cron
import qualified Propellor.Property.Service as Service
import qualified Propellor.Property.User as User
import qualified Propellor.Property.Group as Group
import qualified Propellor.Property.Sudo as Sudo
import qualified Propellor.Property.Borg as Borg
import qualified Propellor.Property.Apache as Apache
import qualified Propellor.Property.Postfix as Postfix
import qualified Propellor.Property.Systemd as Systemd
import qualified Propellor.Property.Network as Network
import qualified Propellor.Property.Fail2Ban as Fail2Ban
import qualified Propellor.Property.LetsEncrypt as LetsEncrypt
import qualified Propellor.Property.Mount as Mount
import Utility.Split

import Data.List
import System.Posix.Files

scrollBox :: Property (HasInfo + DebianLike)
scrollBox = propertyList "scroll server" $ props
	& User.accountFor (User "scroll")
	& Git.cloned (User "scroll") "git://git.kitenet.net/scroll" (d </> "scroll") Nothing
	& Apt.installed ["ghc", "make", "cabal-install", "libghc-vector-dev",
		"libghc-bytestring-dev", "libghc-mtl-dev", "libghc-ncurses-dev",
		"libghc-random-dev", "libghc-monad-loops-dev", "libghc-text-dev",
		"libghc-ifelse-dev", "libghc-case-insensitive-dev",
		"libghc-data-default-dev", "libghc-optparse-applicative-dev"]
	& userScriptProperty (User "scroll")
		[ "cd " ++ d </> "scroll"
		, "git pull"
		, "cabal configure"
		, "make"
		]
		`assume` MadeChange
	& s `File.hasContent`
		[ "#!/bin/sh"
		, "set -e"
		, "echo Preparing to run scroll!"
		, "cd " ++ d
		, "mkdir -p tmp"
		, "TMPDIR= t=$(tempfile -d tmp)"
		, "export t"
		, "rm -f \"$t\""
		, "mkdir \"$t\""
		, "cd \"$t\""
		, "echo"
		, "echo Note that games on this server are time-limited to 2 hours"
		, "echo 'Need more time? Run scroll locally instead!'"
		, "echo"
		, "echo Press Enter to start the game."
		, "read me"
		, "SHELL=/bin/sh script --timing=timing -c " ++ g
		] `onChange` (s `File.mode` (combineModes (ownerWriteMode:readModes ++ executeModes)))
	& g `File.hasContent`
		[ "#!/bin/sh"
		, "if ! timeout --kill-after 1m --foreground 2h ../../scroll/scroll; then"
		, "echo Scroll seems to have ended unexpectedly. Possibly a bug.."
		, "else"
		, "echo Thanks for playing scroll! https://joeyh.name/code/scroll/"
		, "fi"
		, "echo Your game was recorded, as ID:$(basename \"$t\")"
		, "echo if you would like to talk about how it went, email scroll@joeyh.name"
		, "read line"
		] `onChange` (g `File.mode` (combineModes (ownerWriteMode:readModes ++ executeModes)))
	-- prevent port forwarding etc by not letting scroll log in via ssh
	& Ssh.sshdConfig `File.containsLine` ("DenyUsers scroll")
		`onChange` Ssh.restarted
	& User.shellSetTo (User "scroll") s
	& User.hasPassword (User "scroll")
	-- telnetd attracted password crackers, so disabled
	& Apt.removed ["telnetd"]
	& Apt.installed ["shellinabox"]
	& File.hasContent "/etc/default/shellinabox"
		[ "# Deployed by propellor"
		, "SHELLINABOX_DAEMON_START=1"
		, "SHELLINABOX_PORT=4242"
		, "SHELLINABOX_ARGS=\"--disable-ssl --no-beep --service=:scroll:scroll:" ++ d ++ ":" ++ s ++ "\""
		]
		`onChange` Service.restarted "shellinabox"
	& Service.running "shellinabox"
  where
	d = "/home/scroll"
	s = d </> "login.sh"
	g = d </> "game.sh"

oldUseNetServer :: [Host] -> Property (HasInfo + DebianLike)
oldUseNetServer hosts = propertyList "olduse.net server" $ props
	& Apt.installed ["leafnode"]
	& oldUseNetInstalled "oldusenet-server"
	& oldUseNetBackup
	& spoolsymlink
	& "/etc/news/leafnode/config" `File.hasContent`
		[ "# olduse.net configuration (deployed by propellor)"
		, "expire = 1000000" -- no expiry via texpire
		, "server = " -- no upstream server
		, "debugmode = 1"
		, "allowSTRANGERS = 42" -- lets anyone connect
		, "nopost = 1" -- no new posting (just gather them)
		]
	& "/etc/hosts.deny" `File.lacksLine` "leafnode: ALL"
	& Apt.serviceInstalledRunning "openbsd-inetd"
	& File.notPresent "/etc/cron.daily/leafnode"
	& File.notPresent "/etc/cron.d/leafnode"
	& Cron.niceJob "oldusenet-expire" (Cron.Times "11 1 * * *") (User "news") newsspool expirecommand
	& Cron.niceJob "oldusenet-uucp" (Cron.Times "*/5 * * * *") (User "news") "/" uucpcommand
	& Apache.siteEnabled "nntp.olduse.net" nntpcfg
  where
	newsspool = "/var/spool/news"
	datadir = "/var/spool/oldusenet"
	expirecommand = intercalate ";"
		[ "find \\( -path ./out.going -or -path ./interesting.groups -or -path './*/.overview' \\) -prune -or -type f -ctime +60  -print | xargs --no-run-if-empty rm"
		, "find -type d -empty | xargs --no-run-if-empty rmdir"
		]
	uucpcommand = "/usr/bin/uucp " ++ datadir
	nntpcfg = apachecfg "nntp.olduse.net"
		[ "  DocumentRoot " ++ datadir ++ "/"
		, "  <Directory " ++ datadir ++ "/>"
		, "    Options Indexes FollowSymlinks"
		, "    AllowOverride None"
		, Apache.allowAll
		, "  </Directory>"
		]

	spoolsymlink :: Property UnixLike
	spoolsymlink = check (not . isSymbolicLink <$> getSymbolicLinkStatus newsspool)
		(property "olduse.net spool in place" $ makeChange $ do
			removeDirectoryRecursive newsspool
			createSymbolicLink (datadir </> "news") newsspool
		)

	oldUseNetBackup :: Property (HasInfo + DebianLike)
	oldUseNetBackup = Borg.backup datadir borgrepo 
		(Cron.Times "33 4 * * *")
		[]
		[Borg.KeepDays 30]
		`requires` Ssh.userKeyAt (Just keyfile)
			(User "root")
			(Context "olduse.net")
			(SshRsa, "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQD0F6L76SChMCIGmeyGhlFMUTgZ3BoTbATiOSs0A7KXQoI1LTE5ZtDzzUkrQRJVpJ640pfMR7cQZyBm8tv+kYIPp0238GrX43c1vgm0L78agDnBU7r2iNMyWIwhssK8O3ZAhp8Q4KCz1r8hP2nIiD0y1D1VWW8h4KWOS7I1XCEAjOTvFvEjTh6a9MyHrcIkv7teUUzTBRjNrsyijCFRk1+pEET54RueoOmEjQcWd/sK1tYRiMZjegRLBOus2wUWsUOvznJ2iniLONUTGAWRnEV+O7hLN6CD44osJ+wkZk8bPAumTS0zcSLckX1jpdHJicmAyeniWSd4FCqm1YE6/xDD")
		`requires` Ssh.knownHost hosts "usw-s002.rsync.net" (User "root")
	borgrepo = rsyncNetBorgRepo "olduse.net.borg" [Borg.UseSshKey keyfile]
	keyfile = "/root/.ssh/olduse.net.key"

oldUseNetShellBox :: Property DebianLike
oldUseNetShellBox = propertyList "olduse.net shellbox" $ props
	& oldUseNetInstalled "oldusenet"
	& Service.running "shellinabox"

oldUseNetInstalled :: Apt.Package -> Property DebianLike
oldUseNetInstalled pkg = check (not <$> Apt.isInstalled pkg) $
	propertyList ("olduse.net " ++ pkg) $ props
		& Apt.installed (words "build-essential devscripts debhelper git libncursesw5-dev libpcre3-dev pkg-config bison libicu-dev libidn11-dev libcanlock2-dev libuu-dev ghc libghc-ifelse-dev libghc-hxt-dev libghc-utf8-string-dev libghc-missingh-dev libghc-sha-dev haskell-stack")
			`describe` "olduse.net build deps"
		& scriptProperty
			[ "rm -rf /root/tmp/oldusenet" -- idenpotency
			, "git clone git://olduse.net/ /root/tmp/oldusenet/source"
			, "cd /root/tmp/oldusenet/source/"
			, "HOME=/root dpkg-buildpackage -us -uc"
			, "dpkg -i ../" ++ pkg ++ "_*.deb || true"
			, "apt-get -fy install" -- dependencies
			, "rm -rf /root/tmp/oldusenet"
			]
			`assume` MadeChange
			`describe` "olduse.net built"

kgbServer :: Property (HasInfo + DebianLike)
kgbServer = propertyList desc $ props
	& Apt.serviceInstalledRunning "kgb-bot"
	& "/etc/default/kgb-bot" `File.containsLine` "BOT_ENABLED=1"
		`describe` "kgb bot enabled"
		`onChange` Service.running "kgb-bot"
	& File.hasPrivContent "/etc/kgb-bot/kgb.conf" anyContext
		`onChange` Service.restarted "kgb-bot"
  where
	desc = "kgb.kitenet.net setup"

-- git.kitenet.net and git.joeyh.name
gitServer :: [Host] -> Property (HasInfo + DebianLike)
gitServer hosts = propertyList "git.kitenet.net setup" $ props
	& Borg.backup "/srv/git" borgrepo
		(Cron.Times "33 3 * * *")
		[]
		[Borg.KeepDays 30]
		`requires` Ssh.userKeyAt (Just sshkey)
			(User "root")
			(Context "git.kitenet.net")
			(SshRsa, "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDLwUUkpkI9c2Wcnv/E4v9bJ7WcpiNkToltXfzRDd1F31AYrucfSMgzu3rtDpEL+wSnQLua/taJkWUWT/pyXOAh+90K6O/YeBZmY5CK01rYDz3kSTAtwHkMqednsRjdQS6NNJsuWc1reO8a4pKtsToJ3G9VAKufCkt2b8Nhqz0yLvLYwwU/mdI8DmfX6IgXhdy9njVEG/jsQnLFXY6QEfwKbIPs9O6qo4iFJg3defXX+zVMLsh3NE1P2i2VxMjxJEQdPdy9Z1sVpkiQM+mgJuylQQ5flPK8sxhO9r4uoK/JROkjPJNYoJMlsN+QlK04ABb7JV2JwhAL/Y8ypjQ13JdT")
		`requires` Ssh.knownHost hosts "usw-s002.rsync.net" (User "root")
	& Ssh.authorizedKeys (User "family") (Context "git.kitenet.net")
	& User.accountFor (User "family")
	& Apt.installed ["git", "rsync", "cgit"]
	& Apt.installed ["git-annex"]
	& Apt.installed ["kgb-client"]
	& File.hasPrivContentExposed "/etc/kgb-bot/kgb-client.conf" anyContext
		`requires` File.dirExists "/etc/kgb-bot/"
	& Git.daemonRunning "/srv/git"
	& "/etc/cgitrc" `File.hasContent`
		[ "clone-url=https://git.joeyh.name/git/$CGIT_REPO_URL git://git.joeyh.name/$CGIT_REPO_URL"
		, "css=/cgit-css/cgit.css"
		, "logo=/cgit-css/cgit.png"
		, "enable-http-clone=1"
		, "root-title=Joey's git repositories"
		, "root-desc="
		, "enable-index-owner=0"
		, "snapshots=tar.gz"
		, "enable-git-config=1"
		, "scan-path=/srv/git"
		]
		`describe` "cgit configured"
	-- I keep the website used for git.kitenet.net/git.joeyh.name checked into git..
	& Git.cloned (User "joey") "/srv/git/joey/git.kitenet.net.git" "/srv/web/git.kitenet.net" Nothing
	-- Don't need global apache configuration for cgit.
	! Apache.confEnabled "cgit"
	& website "git.kitenet.net"
	& website "git.joeyh.name"
	& Apache.modEnabled "cgi"
  where
	sshkey = "/root/.ssh/git.kitenet.net.key"
	borgrepo = rsyncNetBorgRepo "git.kitenet.net.borg" [Borg.UseSshKey sshkey]
	website hn = Apache.httpsVirtualHost' hn "/srv/web/git.kitenet.net/" letos
		[ Apache.iconDir
		, "  <Directory /srv/web/git.kitenet.net/>"
		, "    Options Indexes ExecCGI FollowSymlinks"
		, "    AllowOverride None"
		, "    AddHandler cgi-script .cgi"
		, "    DirectoryIndex index.cgi"
		,      Apache.allowAll
		, "  </Directory>"
		, ""
		, "  ScriptAlias /cgi-bin/ /usr/lib/cgi-bin/"
		, "  <Directory /usr/lib/cgi-bin>"
		, "    SetHandler cgi-script"
		, "    Options ExecCGI"
		, "  </Directory>"
		]

type AnnexUUID = String

-- | A website, with files coming from a git-annex repository.
annexWebSite :: Git.RepoUrl -> HostName -> AnnexUUID -> [(String, Git.RepoUrl)] -> Property (HasInfo + DebianLike)
annexWebSite origin hn uuid remotes = propertyList (hn ++" website using git-annex") $ props
	& Git.cloned (User "joey") origin dir Nothing
		`onChange` setup
	& alias hn
	& postupdatehook `File.hasContent`
		[ "#!/bin/sh"
		, "exec git update-server-info"
		] `onChange`
			(postupdatehook `File.mode` (combineModes (ownerWriteMode:readModes ++ executeModes)))
	& setupapache
  where
	dir = "/srv/web/" ++ hn
	postupdatehook = dir </> ".git/hooks/post-update"
	setup = userScriptProperty (User "joey") setupscript
		`assume` MadeChange
	setupscript =
		[ "cd " ++ shellEscape dir
		, "git annex reinit " ++ shellEscape uuid
		] ++ map addremote remotes ++
		[ "git annex get"
		, "git update-server-info"
		]
	addremote (name, url) = "git remote add " ++ shellEscape name ++ " " ++ shellEscape url
	setupapache = Apache.httpsVirtualHost' hn dir letos
		[ "  ServerAlias www."++hn
		,    Apache.iconDir
		, "  <Directory "++dir++">"
		, "    Options Indexes FollowSymLinks ExecCGI"
		, "    AllowOverride None"
		, "    AddHandler cgi-script .cgi"
		, "    DirectoryIndex index.html index.cgi"
		,      Apache.allowAll
		, "  </Directory>"
		]

letos :: LetsEncrypt.AgreeTOS
letos = LetsEncrypt.AgreeTOS (Just "id@joeyh.name")

apacheSite :: HostName -> Apache.ConfigFile -> RevertableProperty DebianLike DebianLike
apacheSite hn middle = Apache.siteEnabled hn $ apachecfg hn middle

apachecfg :: HostName -> Apache.ConfigFile -> Apache.ConfigFile
apachecfg hn middle =
	[ "<VirtualHost *:" ++ val port ++ ">"
	, "  ServerAdmin grue@joeyh.name"
	, "  ServerName "++hn++":" ++ val port
	]
	++ middle ++
	[ ""
	, "  ErrorLog /var/log/apache2/error.log"
	, "  LogLevel warn"
	, "  CustomLog /var/log/apache2/access.log combined"
	, "  ServerSignature On"
	, "  "
	, Apache.iconDir
	, "</VirtualHost>"
	]
	  where
		port = Port 80

gitAnnexDistributor :: Property (HasInfo + DebianLike)
gitAnnexDistributor = combineProperties "git-annex distributor, including rsync server and signer" $ props
	& Apt.installed ["rsync"]
	& File.hasPrivContent "/etc/rsyncd.conf" (Context "git-annex distributor")
		`onChange` Service.restarted "rsync"
	& File.hasPrivContent "/etc/rsyncd.secrets" (Context "git-annex distributor")
		`onChange` Service.restarted "rsync"
	& "/etc/default/rsync" `File.containsLine` "RSYNC_ENABLE=true"
		`onChange` Service.running "rsync"
	& Systemd.enabled "rsync"
	& endpoint "/srv/web/downloads.kitenet.net/git-annex/autobuild"
	& endpoint "/srv/web/downloads.kitenet.net/git-annex/autobuild/x86_64-apple-yosemite"
	& endpoint "/srv/web/downloads.kitenet.net/git-annex/autobuild/windows"
	-- git-annex distribution signing key
	& Gpg.keyImported (Gpg.GpgKeyId "89C809CB") (User "joey")
  where
	endpoint d = combineProperties ("endpoint " ++ d) $ props
		& File.dirExists d
		& File.ownerGroup d (User "joey") (Group "joey")

downloads :: Property (HasInfo + DebianLike)
downloads = annexWebSite "/srv/git/downloads.git"
	"downloads.kitenet.net"
	"840760dc-08f0-11e2-8c61-576b7e66acfd"
	[]

tmp :: Property (HasInfo + DebianLike)
tmp = propertyList "tmp.joeyh.name" $ props
	& annexWebSite "/srv/git/joey/tmp.git"
		"tmp.joeyh.name"
		"26fd6e38-1226-11e2-a75f-ff007033bdba"
		[]
	& Cron.jobDropped "pump rss" (Cron.Times "15 * * * *")

ircBouncer :: Property (HasInfo + DebianLike)
ircBouncer = propertyList "IRC bouncer" $ props
	& Apt.installed ["znc"]
	& User.accountFor (User "znc")
	& File.dirExists (takeDirectory conf)
	& File.hasPrivContent conf anyContext
	& File.ownerGroup conf (User "znc") (Group "znc")
	& Cron.job "znconboot" (Cron.Times "@reboot") (User "znc") "~" "znc"
	-- ensure running if it was not already
	& userScriptProperty (User "znc") ["znc || true"]
		`assume` NoChange
		`describe` "znc running"
  where
	conf = "/home/znc/.znc/configs/znc.conf"

githubBackup :: Property (HasInfo + DebianLike)
githubBackup = propertyList "github-backup box" $ props
	& Apt.installed ["github-backup", "moreutils"]
	& githubKeys
	& Cron.niceJob "github-backup run" (Cron.Times "30 4 * * *") (User "joey")
		"/home/joey/lib/backup" backupcmd
  where
	backupcmd = intercalate "&&" $
		[ "mkdir -p github"
		, "cd github"
		, ". $HOME/.github-keys"
		, "github-backup joeyh"
		]

githubKeys :: Property (HasInfo + UnixLike)
githubKeys =
	let f = "/home/joey/.github-keys"
	in File.hasPrivContent f anyContext
		`onChange` File.ownerGroup f (User "joey") (Group "joey")


rsyncNetBackup :: [Host] -> Property DebianLike
rsyncNetBackup hosts = Cron.niceJob "rsync.net copied in daily" (Cron.Times "30 5 * * *")
	(User "joey") "/home/joey/lib/backup" "mkdir -p rsync.net && rsync --delete -az 2318@usw-s002.rsync.net: rsync.net"
	`requires` Ssh.knownHost hosts "usw-s002.rsync.net" (User "joey")

podcatcher :: Property DebianLike
podcatcher = Cron.niceJob "podcatcher run hourly" (Cron.Times "55 * * * *")
	(User "joey") "/home/joey/lib/sound/podcasts"
	"xargs git-annex importfeed -c annex.genmetadata=true < feeds; mr --quiet update"
	`requires` Apt.installed ["git-annex", "myrepos"]

spamdEnabled :: Property DebianLike
spamdEnabled = tightenTargets $ 
	cmdProperty "update-rc.d" ["spamassassin", "enable"]
		`assume` MadeChange

spamassassinConfigured :: Property DebianLike
spamassassinConfigured = propertyList "spamassassin configured" $ props
	& Apt.serviceInstalledRunning "spamassassin"
	& "/etc/default/spamassassin" `File.containsLines`
		[ "# Propellor deployed"
		, "OPTIONS=\"--create-prefs --max-children 5 --helper-home-dir\""
		, "CRON=1"
		, "NICE=\"--nicelevel 15\""
		]
		`describe` "spamd configured"
		`onChange` spamdEnabled
		`onChange` Service.restarted "spamassassin"
		`requires` Apt.serviceInstalledRunning "cron"

kiteMailServer :: Property (HasInfo + DebianLike)
kiteMailServer = propertyList "kitenet.net mail server" $ props
	& Postfix.installed
	& Apt.installed ["postfix-pcre"]
	& Apt.serviceInstalledRunning "postgrey"
	& spamassassinConfigured
	& Apt.serviceInstalledRunning "spamass-milter"
	-- Add -m to prevent modifying messages Subject or body.
	& "/etc/default/spamass-milter" `File.containsLine`
		"OPTIONS=\"-m -u spamass-milter -i 127.0.0.1\""
		`onChange` Service.restarted "spamass-milter"
		`describe` "spamass-milter configured"

	& Apt.serviceInstalledRunning "amavisd-milter"
	& "/etc/default/amavisd-milter" `File.containsLines`
		[ "# Propellor deployed"
		, "MILTERSOCKET=/var/spool/postfix/amavis/amavis.sock"
		, "MILTERSOCKETOWNER=\"postfix:postfix\""
		, "MILTERSOCKETMODE=\"0660\""
		]
		`onChange` Service.restarted "amavisd-milter"
		`describe` "amavisd-milter configured for postfix"
	& Apt.serviceInstalledRunning "clamav-freshclam"
	-- Workaround https://bugs.debian.org/569150
	& Cron.niceJob "amavis-expire" Cron.Daily (User "root") "/"
		"find /var/lib/amavis/virusmails/ -type f -ctime +7 -delete"

	& dkimInstalled

	& Postfix.saslAuthdInstalled
	& Fail2Ban.installed
	& Fail2Ban.jailEnabled "postfix-sasl"
	& "/etc/default/saslauthd" `File.containsLine` "MECHANISMS=sasldb"
	& Postfix.saslPasswdSet "kitenet.net" (User "errol")
	& Postfix.saslPasswdSet "kitenet.net" (User "joey")

	& Apt.installed ["maildrop"]
	& "/etc/maildroprc" `File.hasContent`
		[ "# Global maildrop filter file (deployed with propellor)"
		, "DEFAULT=\"$HOME/Maildir\""
		, "MAILBOX=\"$DEFAULT/.\""
		, "# Filter spam to a spam folder, unless .keepspam exists"
		, "if (/^X-Spam-Status: Yes/)"
		, "{"
		, "  `test -e \"$HOME/.keepspam\"`"
		, "  if ( $RETURNCODE != 0 )"
		, "  to ${MAILBOX}spam"
		, "}"
		]
		`describe` "maildrop configured"

	& "/etc/aliases" `File.hasPrivContentExposed` ctx
		`onChange` Postfix.newaliases
	& hasPostfixCert ctx

	& "/etc/postfix/mydomain" `File.containsLines`
		[ "/.*\\.kitenet\\.net/\tOK"
		, "/ikiwiki\\.info/\tOK"
		, "/joeyh\\.name/\tOK"
		]
		`onChange` Postfix.reloaded
		`describe` "postfix mydomain file configured"
	& "/etc/postfix/obscure_client_relay.pcre" `File.hasContent`
		-- Remove received lines for mails relayed from trusted
		-- clients. These can be a privacy violation, or trigger
		-- spam filters.
		[ "/^Received: from ([^.]+)\\.kitenet\\.net.*using TLS.*by kitenet\\.net \\(([^)]+)\\) with (E?SMTPS?A?) id ([A-F[:digit:]]+)(.*)/ IGNORE"
		-- Munge local Received line for postfix running on a
		-- trusted client that relays through. These can trigger
		-- spam filters.
		, "/^Received: by ([^.]+)\\.kitenet\\.net.*/ REPLACE X-Question: 42"
		]
		`onChange` Postfix.reloaded
		`describe` "postfix obscure_client_relay file configured"
	& Postfix.mappedFile "/etc/postfix/virtual"
		(flip File.containsLines
			[ "# *@joeyh.name to joey"
			, "@joeyh.name\tjoey"
			]
		) `describe` "postfix virtual file configured"
		`onChange` Postfix.reloaded
	& Postfix.mappedFile "/etc/postfix/relay_clientcerts"
		(flip File.hasPrivContentExposed ctx)
	& Postfix.mainCfFile `File.containsLines`
		[ "myhostname = kitenet.net"
		, "mydomain = $myhostname"
		, "append_dot_mydomain = no"
		, "myorigin = kitenet.net"
		, "mydestination = $myhostname, localhost.$mydomain, $mydomain, kite.$mydomain., localhost, regexp:$config_directory/mydomain"
		, "mailbox_command = maildrop"
		, "virtual_alias_maps = hash:/etc/postfix/virtual"

		, "# Allow clients with trusted certs to relay mail through."
		, "relay_clientcerts = hash:/etc/postfix/relay_clientcerts"
		, "smtpd_relay_restrictions = permit_mynetworks,permit_tls_clientcerts,permit_sasl_authenticated,reject_unauth_destination"

		, "# Filter out client relay lines from headers."
		, "header_checks = pcre:$config_directory/obscure_client_relay.pcre"

		, "# Password auth for relaying"
		, "smtpd_sasl_auth_enable = yes"
		, "smtpd_sasl_security_options = noanonymous"
		, "smtpd_sasl_local_domain = kitenet.net"

		, "# Enable postgrey and sasl auth and client certs."
		, "smtpd_recipient_restrictions = permit_tls_clientcerts,permit_sasl_authenticated,,permit_mynetworks,reject_unauth_destination,check_policy_service inet:127.0.0.1:10023"

		, "# Enable spamass-milter, amavis-milter (opendkim is not enabled because it causes mails forwarded from eg gmail to be rejected)"
		, "smtpd_milters = unix:/spamass/spamass.sock unix:amavis/amavis.sock"
		, "# opendkim is used for outgoing mail"
		, "non_smtpd_milters = inet:localhost:8891"
		, "milter_connect_macros = j {daemon_name} v {if_name} _"
		, "# If a milter is broken, fall back to just accepting mail."
		, "milter_default_action = accept"

		, "# TLS setup -- server"
		, "smtpd_tls_CAfile = /etc/ssl/certs/joeyca.pem"
		, "smtpd_tls_cert_file = /etc/ssl/certs/postfix.pem"
		, "smtpd_tls_key_file = /etc/ssl/private/postfix.pem"
		, "smtpd_tls_loglevel = 1"
		, "smtpd_tls_received_header = yes"
		, "smtpd_use_tls = yes"
		, "smtpd_tls_ask_ccert = yes"
		, "smtpd_tls_session_cache_database = sdbm:/etc/postfix/smtpd_scache"

		, "# TLS setup -- client"
		, "smtp_tls_CAfile = /etc/ssl/certs/joeyca.pem"
		, "smtp_tls_cert_file = /etc/ssl/certs/postfix.pem"
		, "smtp_tls_key_file = /etc/ssl/private/postfix.pem"
		, "smtp_tls_loglevel = 1"
		, "smtp_use_tls = yes"
		, "smtp_tls_session_cache_database = sdbm:/etc/postfix/smtp_scache"

		, "# Allow larger attachments, up to 200 mb."
		, "# (Avoid setting too high; the postfix queue must have"
		, "# 1.5 times this much space free, or postfix will reject"
		, "# ALL mail!)"
		, "message_size_limit = 204800000"
		, "virtual_mailbox_limit = 20480000"
		]
		`onChange` Postfix.dedupMainCf
		`onChange` Postfix.reloaded
		`describe` "postfix configured"

	& Apt.serviceInstalledRunning "dovecot-imapd"
	& Apt.serviceInstalledRunning "dovecot-pop3d"
	& "/etc/dovecot/conf.d/10-mail.conf" `File.containsLine`
		"mail_location = maildir:~/Maildir"
		`onChange` Service.reloaded "dovecot"
		`describe` "dovecot mail.conf"
	& "/etc/dovecot/conf.d/10-auth.conf" `File.containsLine`
		"!include auth-passwdfile.conf.ext"
		`onChange` Service.restarted "dovecot"
		`describe` "dovecot auth.conf"
	& File.hasPrivContent dovecotusers ctx
		`onChange` (dovecotusers `File.mode`
			combineModes [ownerReadMode, groupReadMode])
	& File.ownerGroup dovecotusers (User "root") (Group "dovecot")

	& Apt.installed ["mutt", "bsd-mailx", "alpine"]

	& pinescript `File.hasContent`
		[ "#!/bin/sh"
		, "# deployed with propellor"
		, "set -e"
		, "exec alpine \"$@\""
		]
		`onChange` (pinescript `File.mode`
			combineModes (readModes ++ executeModes))
		`describe` "pine wrapper script"
	-- Make pine use dovecot pipe to read maildir.
	& "/etc/pine.conf" `File.hasContent`
		[ "# deployed with propellor"
		, "inbox-path={localhost}inbox"
		, "rsh-command=" ++ imapalpinescript
		]
		`describe` "pine configured to use local imap server"
	& imapalpinescript `File.hasContent`
		[ "#!/bin/sh"
		, "# deployed with propellor"
		, "set -e"
		, "exec /usr/lib/dovecot/imap 2>/dev/null"
		]
		`onChange` (imapalpinescript `File.mode`
			combineModes (readModes ++ executeModes))
		`describe` "imap script for pine"
	& Apt.serviceInstalledRunning "mailman"
	-- Override the default http url. (Only affects new lists.)
	& "/etc/mailman/mm_cfg.py" `File.containsLine`
		"DEFAULT_URL_PATTERN = 'https://%s/cgi-bin/mailman/'"

	& Postfix.service ssmtp

	& Apt.installed ["fetchmail"]
  where
	ctx = Context "kitenet.net"
	pinescript = "/usr/local/bin/pine"
	imapalpinescript = "/usr/local/bin/imap-for-alpine"
	dovecotusers = "/etc/dovecot/users"

	ssmtp = Postfix.Service
		(Postfix.InetService Nothing "ssmtp")
		"smtpd" Postfix.defServiceOpts

-- Configures postfix to have the dkim milter, and no other milters.
dkimMilter :: Property (HasInfo + DebianLike)
dkimMilter = Postfix.mainCfFile `File.containsLines`
	[ "smtpd_milters = inet:localhost:8891"
	, "non_smtpd_milters = inet:localhost:8891"
	, "milter_default_action = accept"
	]
	`describe` "postfix dkim milter"
	`onChange` Postfix.dedupMainCf
	`onChange` Postfix.reloaded
	`requires` dkimInstalled
	`requires` Postfix.installed

-- This does not configure postfix to use the dkim milter,
-- nor does it set up domainkey DNS.
dkimInstalled :: Property (HasInfo + DebianLike)
dkimInstalled = go `onChange` Service.restarted "opendkim"
  where
	go = propertyList "opendkim installed" $ props
		& Apt.serviceInstalledRunning "opendkim"
		& File.dirExists "/etc/mail"
		& File.hasPrivContent "/etc/mail/dkim.key" (Context "kitenet.net")
		& File.ownerGroup "/etc/mail/dkim.key" (User "root") (Group "root")
		& "/etc/default/opendkim" `File.containsLine`
			"SOCKET=\"inet:8891@localhost\""
			`onChange` 
				(cmdProperty "/lib/opendkim/opendkim.service.generate" []
				`assume` MadeChange)
			`onChange` Service.restarted "opendkim"
		& "/etc/opendkim.conf" `File.containsLines`
			[ "KeyFile /etc/mail/dkim.key"
			, "SubDomains yes"
			, "Domain *"
			, "Selector mail"
			]

-- This is the dkim public key, corresponding with /etc/mail/dkim.key
-- This value can be included in a domain's additional records to make
-- it use this domainkey.
domainKey :: (BindDomain, Record)
domainKey = (RelDomain "mail._domainkey", TXT "v=DKIM1; k=rsa; p=MIGfMA0GCSqGSIb3DQEBAQUAA4GNADCBiQKBgQCc+/rfzNdt5DseBBmfB3C6sVM7FgVvf4h1FeCfyfwPpVcmPdW6M2I+NtJsbRkNbEICxiP6QY2UM0uoo9TmPqLgiCCG2vtuiG6XMsS0Y/gGwqKM7ntg/7vT1Go9vcquOFFuLa5PnzpVf8hB9+PMFdS4NPTvWL2c5xxshl/RJzICnQIDAQAB")

postfixSaslPasswordClient :: Property (HasInfo + DebianLike)
postfixSaslPasswordClient = combineProperties "postfix uses SASL password to authenticate with smarthost" $ props
	& Postfix.mappedFile "/etc/postfix/sasl_passwd" 
		(`File.hasPrivContent` (Context "kitenet.net"))
	& Postfix.mainCfFile `File.containsLines`
		[ "# TLS setup for SASL auth to kite"
		, "smtp_sasl_auth_enable = yes"
		, "smtp_tls_security_level = encrypt"
		, "smtp_sasl_tls_security_options = noanonymous"
		, "relayhost = [kitenet.net]"
		, "smtp_sasl_password_maps = hash:/etc/postfix/sasl_passwd"
		]
		`onChange` Postfix.reloaded
	-- Comes after so it does not set relayhost but uses the setting 
	-- above.
	& Postfix.satellite

hasPostfixCert :: Context -> Property (HasInfo + UnixLike)
hasPostfixCert ctx = combineProperties "postfix tls cert installed" $ props
	& "/etc/ssl/certs/postfix.pem" `File.hasPrivContentExposed` ctx
	& "/etc/ssl/private/postfix.pem" `File.hasPrivContent` ctx

-- Legacy static web sites and redirections from kitenet.net to newer
-- sites.
legacyWebSites :: Property (HasInfo + DebianLike)
legacyWebSites = propertyList "legacy web sites" $ props
	& Apt.serviceInstalledRunning "apache2"
	& Apache.modEnabled "rewrite"
	& Apache.modEnabled "cgi"
	& Apache.modEnabled "speling"
	& userDirHtml
	& Apache.httpsVirtualHost' "kitenet.net" "/var/www" letos kitenetcfg
	& alias "anna.kitenet.net"
	& apacheSite "anna.kitenet.net"
		[ "DocumentRoot /home/anna/html"
		, "<Directory /home/anna/html/>"
		, "  Options Indexes ExecCGI"
		, "  AllowOverride None"
		, Apache.allowAll
		, "</Directory>"
		]
	& alias "sows-ear.kitenet.net"
	& alias "www.sows-ear.kitenet.net"
	& apacheSite "sows-ear.kitenet.net"
		[ "ServerAlias www.sows-ear.kitenet.net"
		, "DocumentRoot /srv/web/sows-ear.kitenet.net"
		, "<Directory /srv/web/sows-ear.kitenet.net>"
		, "  Options FollowSymLinks"
		, "  AllowOverride None"
		, Apache.allowAll
		, "</Directory>"
		, "RewriteEngine On"
		, "RewriteRule .* http://www.sowsearpoetry.org/ [L]"
		]
	& alias "wortroot.kitenet.net"
	& alias "www.wortroot.kitenet.net"
	& apacheSite "wortroot.kitenet.net"
		[ "ServerAlias www.wortroot.kitenet.net"
		, "DocumentRoot /srv/web/wortroot.kitenet.net"
		, "<Directory /srv/web/wortroot.kitenet.net>"
		, "  Options FollowSymLinks"
		, "  AllowOverride None"
		, Apache.allowAll
		, "</Directory>"
		]
	& alias "creeksidepress.com"
	& apacheSite "creeksidepress.com"
		[ "ServerAlias www.creeksidepress.com"
		, "DocumentRoot /srv/web/www.creeksidepress.com"
		, "<Directory /srv/web/www.creeksidepress.com>"
		, "  Options FollowSymLinks"
		, "  AllowOverride None"
		, Apache.allowAll
		, "</Directory>"
		]
	& alias "joey.kitenet.net"
	& apacheSite "joey.kitenet.net"
		[ "DocumentRoot /var/www"
		, "<Directory /var/www/>"
		, "  Options Indexes ExecCGI"
		, "  AllowOverride None"
		, Apache.allowAll
		, "</Directory>"

		, "RewriteEngine On"

		, "# Old ikiwiki filenames for joey's wiki."
		, "rewritecond $1 !.*/index$"
		, "rewriterule (.+).html$ http://joeyh.name/$1/ [l]"

		, "rewritecond $1 !.*/index$"
		, "rewriterule (.+).rss$ http://joeyh.name/$1/index.rss [l]"

		, "# Redirect all to joeyh.name."
		, "rewriterule (.*) http://joeyh.name$1 [r]"
		]
	& alias "homepower.joeyh.name"
	& apacheSite "homepower.joeyh.name"
		[ "DocumentRoot /srv/web/homepower.joeyh.name"
		, "<Directory /srv/web/homepower.joeyh.name>"
		, "  Options Indexes ExecCGI"
		, "  AllowOverride None"
		, Apache.allowAll
		, "</Directory>"
		]
  where
	kitenetcfg =
		-- /var/www is empty
		[ "DocumentRoot /var/www"
		, "<Directory /var/www>"
		, "  Options Indexes FollowSymLinks MultiViews ExecCGI Includes"
		, "  AllowOverride None"
		, Apache.allowAll
		, "</Directory>"
		, "ScriptAlias /cgi-bin/ /usr/lib/cgi-bin/"

		-- for mailman cgi scripts
		, "<Directory /usr/lib/cgi-bin>"
		, "  AllowOverride None"
		, "  Options ExecCGI"
		, Apache.allowAll
		, "</Directory>"
		, "Alias /pipermail/ /var/lib/mailman/archives/public/"
		, "<Directory /var/lib/mailman/archives/public/>"
		, "  Options Indexes MultiViews FollowSymlinks"
		, "  AllowOverride None"
		, Apache.allowAll
		, "</Directory>"
		, "Alias /images/ /usr/share/images/"
		, "<Directory /usr/share/images/>"
		, "  Options Indexes MultiViews"
		, "  AllowOverride None"
		, Apache.allowAll
		, "</Directory>"

		, "RewriteEngine On"
		, "# Force hostname to kitenet.net"
		, "RewriteCond %{HTTP_HOST} !^kitenet\\.net [NC]"
		, "RewriteCond %{HTTP_HOST} !^$"
		, "RewriteRule ^/(.*) http://kitenet\\.net/$1 [L,R]"

		, "# Moved pages"
		, "RewriteRule /programs/debhelper http://joeyh.name/code/debhelper/ [L]"
		, "RewriteRule /programs/satutils http://joeyh.name/code/satutils/ [L]"
		, "RewriteRule /programs/filters http://joeyh.name/code/filters/ [L]"
		, "RewriteRule /programs/ticker http://joeyh.name/code/ticker/ [L]"
		, "RewriteRule /programs/pdmenu http://joeyh.name/code/pdmenu/ [L]"
		, "RewriteRule /programs/sleepd http://joeyh.name/code/sleepd/ [L]"
		, "RewriteRule /programs/Lingua::EN::Words2Nums http://joeyh.name/code/Words2Nums/ [L]"
		, "RewriteRule /programs/wmbattery http://joeyh.name/code/wmbattery/ [L]"
		, "RewriteRule /programs/dpkg-repack http://joeyh.name/code/dpkg-repack/ [L]"
		, "RewriteRule /programs/debconf http://joeyh.name/code/debconf/ [L]"
		, "RewriteRule /programs/perlmoo http://joeyh.name/code/perlmoo/ [L]"
		, "RewriteRule /programs/alien http://joeyh.name/code/alien/ [L]"
		, "RewriteRule /~joey/blog/entry/(.+)-[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]-[0-9][0-9]-[0-9][0-9].html http://joeyh.name/blog/entry/$1/ [L]"
		, "RewriteRule /~anna/.* http://waldeneffect\\.org/ [R]"
		, "RewriteRule /~anna/.* http://waldeneffect\\.org/ [R]"
		, "RewriteRule /~anna http://waldeneffect\\.org/ [R]"
		, "RewriteRule /simpleid/ http://openid.kitenet.net:8086/simpleid/"
		, "# Even the kite home page is not here any more!"
		, "RewriteRule ^/$ http://www.kitenet.net/ [R]"
		, "RewriteRule ^/index.html http://www.kitenet.net/ [R]"
		, "RewriteRule ^/joey http://www.kitenet.net/joey/ [R]"
		, "RewriteRule ^/joey/index.html http://www.kitenet.net/joey/ [R]"
		, "RewriteRule ^/wifi http://www.kitenet.net/wifi/ [R]"
		, "RewriteRule ^/wifi/index.html http://www.kitenet.net/wifi/ [R]"

		, "# Old ikiwiki filenames for kitenet.net wiki."
		, "rewritecond $1 !^/~"
		, "rewritecond $1 !^/doc/"
		, "rewritecond $1 !^/pipermail/"
		, "rewritecond $1 !^/cgi-bin/"
		, "rewritecond $1 !.*/index$"
		, "rewriterule (.+).html$ $1/ [r]"

		, "# Old ikiwiki filenames for joey's wiki."
		, "rewritecond $1 ^/~joey/"
		, "rewritecond $1 !.*/index$"
		, "rewriterule (.+).html$ http://kitenet.net/$1/ [L,R]"

		, "# ~joey to joeyh.name"
		, "rewriterule /~joey/(.*) http://joeyh.name/$1 [L]"

		, "# Old familywiki location."
		, "rewriterule /~family/(.*).html http://family.kitenet.net/$1 [L]"
		, "rewriterule /~family/(.*).rss http://family.kitenet.net/$1/index.rss [L]"
		, "rewriterule /~family(.*) http://family.kitenet.net$1 [L]"

		, "rewriterule /~kyle/bywayofscience(.*) http://bywayofscience.branchable.com$1 [L]"
		, "rewriterule /~kyle/family/wiki/(.*).html http://macleawiki.branchable.com/$1 [L]"
		, "rewriterule /~kyle/family/wiki/(.*).rss http://macleawiki.branchable.com/$1/index.rss [L]"
		, "rewriterule /~kyle/family/wiki(.*) http://macleawiki.branchable.com$1 [L]"
		]

userDirHtml :: Property DebianLike
userDirHtml = File.fileProperty "apache userdir is html" (map munge) conf
	`onChange` Apache.reloaded
	`requires` Apache.modEnabled "userdir"
  where
	munge = replace "public_html" "html"
	conf = "/etc/apache2/mods-available/userdir.conf"

-- Alarm clock: see
-- <http://joeyh.name/blog/entry/a_programmable_alarm_clock_using_systemd/>
--
-- oncalendar example value: "*-*-* 7:30"
alarmClock :: String -> User -> String -> Property Linux
alarmClock oncalendar (User user) command = combineProperties "goodmorning timer installed" $ props
	& "/etc/systemd/system/goodmorning.timer" `File.hasContent`
		[ "[Unit]"
		, "Description=good morning"
		, ""
		, "[Timer]"
		, "Unit=goodmorning.service"
		, "OnCalendar=" ++ oncalendar
		, "WakeSystem=true"
		, "Persistent=false"
		, ""
		, "[Install]"
		, "WantedBy=multi-user.target"
		]
		`onChange` (Systemd.daemonReloaded
			`before` Systemd.restarted "goodmorning.timer")
	& "/etc/systemd/system/goodmorning.service" `File.hasContent`
		[ "[Unit]"
		, "Description=good morning"
		, "RefuseManualStart=true"
		, "RefuseManualStop=true"
		, "ConditionACPower=true"
		, "StopWhenUnneeded=yes"
		, ""
		, "[Service]"
		, "Type=oneshot"
		, "ExecStart=/bin/systemd-inhibit --what=handle-lid-switch --why=goodmorning /bin/su " ++ user ++ " -c \"" ++ command ++ "\""
		]
		`onChange` Systemd.daemonReloaded
	& Systemd.enabled "goodmorning.timer"
	& Systemd.started "goodmorning.timer"
	& "/etc/systemd/logind.conf" `ConfFile.containsIniSetting`
		("Login", "LidSwitchIgnoreInhibited", "no")

homePower :: IsContext c => User -> [Host] -> c -> (SshKeyType, Ssh.PubKeyText) -> Property (HasInfo + DebianLike)
homePower user hosts ctx sshkey = propertyList "home power" $ props
	& Apache.installed
	& Apt.installed ["python", "python-pymodbus", "rrdtool", "rsync"]
	& File.ownerGroup "/var/www/html" user (userGroup user)
	& Git.cloned user "https://git.joeyh.name/git/joey/homepower.git" d Nothing
	& Git.cloned user "https://git.joeyh.name/git/reactive-banana-automation.git" (d </> "reactive-banana-automation") Nothing
	& build
	& Systemd.enabled setupservicename
		`requires` setupserviceinstalled
		`onChange` Systemd.started setupservicename
	& Systemd.enabled watchdogservicename
		`requires` watchdogserviceinstalled
		`onChange` Systemd.started watchdogservicename
	& Systemd.enabled pollerservicename
		`requires` pollerserviceinstalled
		`onChange` Systemd.started pollerservicename
	& Systemd.enabled controllerservicename
		`requires` controllerserviceinstalled
		`onChange` Systemd.started controllerservicename
	& User.hasGroup user (Group "dialout")
	& Group.exists (Group "gpio") Nothing
	& User.hasGroup user (Group "gpio")
	& Apt.installed ["i2c-tools"]
	& User.hasGroup user (Group "i2c")
	& "/etc/modules-load.d/homepower.conf" `File.hasContent` ["i2c-dev"]
	& Cron.niceJob "homepower upload"
		(Cron.Times "1 * * * *") user d rsynccommand
		`requires` Ssh.userKeyAt (Just sshkeyfile) user ctx sshkey
		`requires` File.ownerGroup (takeDirectory sshkeyfile)
			user (userGroup user)
		`requires` File.dirExists (takeDirectory sshkeyfile)
		`requires` Ssh.knownHost hosts "kitenet.net" user
	&  File.hasPrivContentExposed "/etc/darksky-forecast-url" anyContext
  where
	d = "/var/www/html/homepower"
	sshkeyfile = d </> ".ssh/key"
	build = check (not <$> doesFileExist (d </> "controller")) $
		userScriptProperty (User "joey")
			[ "cd " ++ d </> "reactive-banana-automation"
			, "cabal install"
			, "cd " ++ d
			, "make"
			]
		`assume` MadeChange
		`requires` Apt.installed
			[ "ghc", "cabal-install", "make"
			, "libghc-http-types-dev"
			, "libghc-aeson-dev"
			, "libghc-wai-dev"
			, "libghc-warp-dev"
			, "libghc-http-client-dev"
			, "libghc-http-client-tls-dev"
			, "libghc-reactive-banana-dev"
			, "libghc-hinotify-dev"
			]
	pollerservicename = "homepower"
	pollerservicefile = "/etc/systemd/system/" ++ pollerservicename ++ ".service"
	pollerserviceinstalled = pollerservicefile `File.hasContent`
		[ "[Unit]"
		, "Description=home power poller"
		, ""
		, "[Service]"
		, "ExecStart=" ++ d ++ "/poller"
		, "WorkingDirectory=" ++ d
		, "User=joey"
		, "Group=joey"
		, "Restart=always"
		, ""
		, "[Install]"
		, "WantedBy=multi-user.target"
		, "WantedBy=homepower-controller.target"
		]
	controllerservicename = "homepower-controller"
	controllerservicefile = "/etc/systemd/system/" ++ controllerservicename ++ ".service"
	controllerserviceinstalled = controllerservicefile `File.hasContent`
		[ "[Unit]"
		, "Description=home power controller"
		, ""
		, "[Service]"
		, "ExecStart=" ++ d ++ "/controller"
		, "WorkingDirectory=" ++ d
		, "User=joey"
		, "Group=joey"
		, "Restart=always"
		, ""
		, "[Install]"
		, "WantedBy=multi-user.target"
		]
	watchdogservicename = "homepower-watchdog"
	watchdogservicefile = "/etc/systemd/system/" ++ watchdogservicename ++ ".service"
	watchdogserviceinstalled = watchdogservicefile `File.hasContent`
		[ "[Unit]"
		, "Description=home power watchdog"
		, ""
		, "[Service]"
		, "ExecStart=" ++ d ++ "/watchdog"
		, "WorkingDirectory=" ++ d
		, "User=root"
		, "Group=root"
		, "Restart=always"
		, ""
		, "[Install]"
		, "WantedBy=multi-user.target"
		]
	setupservicename = "homepower-setup"
	setupservicefile = "/etc/systemd/system/" ++ setupservicename ++ ".service"
	setupserviceinstalled = setupservicefile `File.hasContent`
		[ "[Unit]"
		, "Description=home power monitor setup"
		, ""
		, "[Service]"
		, "ExecStart=" ++ d ++ "/setup"
		, "WorkingDirectory=" ++ d
		, "User=root"
		, "Group=root"
		, "Type=oneshot"
		, ""
		, "[Install]"
		, "WantedBy=multi-user.target"
		, "WantedBy=homepower.target"
		]
	-- Any changes to the rsync command will need my .authorized_keys
	-- rsync server command to be updated too.
	rsynccommand = "rsync -e 'ssh -i" ++ sshkeyfile ++ "' -avz rrds/ joey@kitenet.net:/srv/web/homepower.joeyh.name/rrds/"

homerouterWifiInterfaceOld :: String
homerouterWifiInterfaceOld = "wlx00c0ca82eb78" -- thinkpenguin wifi adapter

homerouterWifiInterface :: String
homerouterWifiInterface = "wlx7cdd90400448" -- small wifi dongle

-- My home router, running hostapd and dnsmasq,
-- with eth0 connected to a satellite modem, and a fallback ppp connection.
homeRouter :: Property (HasInfo + DebianLike)
homeRouter = propertyList "home router" $ props
	& File.notPresent (Network.interfaceDFile homerouterWifiInterfaceOld)
	& Network.static homerouterWifiInterface (IPv4 "10.1.1.1") Nothing
		`requires` Network.cleanInterfacesFile
	& Apt.installed ["hostapd"]
	& File.hasContent "/etc/hostapd/hostapd.conf"
			[ "interface=" ++ homerouterWifiInterface
			, "ssid=house"
			, "hw_mode=g"
			, "channel=8"
			]
		`requires` File.dirExists "/etc/hostapd"
		`requires` File.hasContent "/etc/default/hostapd"
			[ "DAEMON_CONF=/etc/hostapd/hostapd.conf" ]
		`onChange` Service.running "hostapd"
	& Systemd.enabled "hostapd"
	& File.hasContent "/etc/resolv.conf"
		[ "domain kitenet.net"
		, "search kitenet.net"
		, "nameserver 8.8.8.8"
		, "nameserver 8.8.4.4"
		]
	& Apt.installed ["dnsmasq"]
	& File.hasContent "/etc/dnsmasq.conf"
		[ "domain-needed"
		, "bogus-priv"
		, "interface=" ++ homerouterWifiInterface
		, "interface=eth0"
		, "domain=kitenet.net"
		-- lease time is short because the homepower
		-- controller wants to know when clients disconnect
		, "dhcp-range=10.1.1.100,10.1.1.150,10m"
		, "no-hosts"
		, "address=/honeybee.kitenet.net/10.1.1.1"
		, "address=/house.kitenet.net/10.1.1.1"
		, "dhcp-host=0c:98:38:80:6a:f9,10.1.1.134,android-kodama"
		]
		`onChange` Service.restarted "dnsmasq"
	& ipmasq homerouterWifiInterface
	-- Used to bring down eth0 when satellite is off, which causes ppp
	-- to start, but I am not using this currently.
	& Apt.removed ["netplug"]
	& Network.static' "eth0" (IPv4 "192.168.1.100")
		(Just (Network.Gateway (IPv4 "192.168.1.1")))
		-- When satellite is down, fall back to dialup
		[ ("pre-up", "poff -a || true")
		, ("post-down", "pon")
		]
		`requires` Network.cleanInterfacesFile
	& Apt.installed ["ppp"]
		`before` File.hasContent "/etc/ppp/peers/provider"
			[ "user \"joeyh@arczip.com\""
			, "connect \"/usr/sbin/chat -v -f /etc/chatscripts/pap -T 3825441\""
			, "/dev/ttyACM0"
			, "115200"
			, "noipdefault"
			, "defaultroute"
			, "persist"
			, "noauth"
			]
		`before` File.hasPrivContent "/etc/ppp/pap-secrets" (Context "joeyh@arczip.com")

-- | Enable IP masqerading, on whatever other interfaces come up, besides the
-- provided intif.
ipmasq :: String -> Property DebianLike
ipmasq intif = File.hasContent ifupscript
	[ "#!/bin/sh"
	, "INTIF=" ++ intif
	, "if [ \"$IFACE\" = $INTIF ] || [ \"$IFACE\" = lo ]; then"
	, "exit 0"
	, "fi"
	, "iptables -F"
	, "iptables -A FORWARD -i $IFACE -o $INTIF -m state --state ESTABLISHED,RELATED -j ACCEPT"
	, "iptables -A FORWARD -i $INTIF -o $IFACE -j ACCEPT"
	, "iptables -t nat -A POSTROUTING -o $IFACE -j MASQUERADE"
	, "echo 1 > /proc/sys/net/ipv4/ip_forward"
	]
	`before` scriptmode ifupscript
	`before` File.dirExists (takeDirectory pppupscript)
	`before` File.hasContent pppupscript
		[ "#!/bin/sh"
		, "IFACE=$PPP_IFACE " ++ ifupscript
		]
	`before` scriptmode pppupscript
	`requires` Apt.installed ["iptables"]
  where
	ifupscript = "/etc/network/if-up.d/ipmasq"
	pppupscript = "/etc/ppp/ip-up.d/ipmasq"
	scriptmode f = f `File.mode` combineModes (readModes ++ executeModes)

laptopSoftware :: Property DebianLike
laptopSoftware = Apt.installed
	[ "intel-microcode", "acpi"
	, "procmeter3", "xfce4", "procmeter3", "unclutter"
	, "mplayer", "fbreader", "firefox", "chromium"
	, "libdatetime-event-sunrise-perl", "libtime-duration-perl"
	, "network-manager", "network-manager-openvpn-gnome", "openvpn"
	, "gtk-redshift", "powertop"
	, "gimp", "gthumb", "inkscape", "sozi", "xzgv", "hugin"
	, "mpc", "mpd", "ncmpc", "sonata", "mpdtoys"
	, "bsdgames", "nethack-console"
	, "xmonad", "libghc-xmonad-dev", "libghc-xmonad-contrib-dev"
	, "ttf-bitstream-vera", "fonts-symbola", "fonts-noto-color-emoji"
	, "mairix", "offlineimap", "mutt", "slrn"
	, "mtr", "nmap", "whois", "wireshark", "tcpdump", "iftop"
	, "pmount", "tree", "pv"
	, "arbtt", "hledger", "bc"
	, "apache2", "ikiwiki", "libhighlight-perl"
	, "avahi-daemon", "avahi-discover"
	, "pal"
	, "yeahconsole", "xkbset", "xinput"
	, "assword", "pumpa"
	, "vorbis-tools", "audacity"
	, "ekiga"
	, "bluez-firmware", "blueman", "pulseaudio-module-bluetooth"
	, "xul-ext-ublock-origin", "xul-ext-pdf.js", "xul-ext-status4evar"
	, "vim-syntastic", "vim-fugitive"
	, "adb", "gthumb"
	, "w3m", "sm", "weechat"
	, "borgbackup", "wipe", "smartmontools", "libgfshare-bin"
	, "units"
	, "virtualbox", "virtualbox-guest-additions-iso", "qemu-kvm"
	]
	`requires` baseSoftware
	`requires` devSoftware

baseSoftware :: Property DebianLike
baseSoftware = Apt.installed
	[ "bash", "bash-completion", "vim", "screen", "less", "moreutils"
	, "git", "mr", "etckeeper", "git-annex", "ssh", "vim-vimoutliner"
	]

devSoftware :: Property DebianLike
devSoftware = Apt.installed
	[ "build-essential", "debhelper", "devscripts"
	, "ghc", "cabal-install", "haskell-stack"
	, "hothasktags", "hdevtools", "hlint"
	, "gdb", "time"
	, "dpkg-repack", "lintian"
	, "pristine-tar", "github-backup"
	]

cubieTruckOneWire :: Property DebianLike
cubieTruckOneWire = utilitysetup
	`requires` dtsinstalled
	`requires` utilityinstalled
  where
	dtsinstalled = File.hasContent "/etc/easy-peasy-devicetree-squeezy/my.dts" mydts
		`requires` File.dirExists "/etc/easy-peasy-devicetree-squeezy"
	utilityinstalled = Git.cloned (User "root") "https://git.joeyh.name/git/easy-peasy-devicetree-squeezy.git" "/usr/local/easy-peasy-devicetree-squeezy" Nothing
		`onChange` File.isSymlinkedTo "/usr/local/bin/easy-peasy-devicetree-squeezy" (File.LinkTarget "/usr/local/easy-peasy-devicetree-squeezy/easy-peasy-devicetree-squeezy")
		`requires` Apt.installed ["pv", "device-tree-compiler", "cpp", "linux-source"]
	utilitysetup = check (not <$> doesFileExist dtb) $ 
		cmdProperty "easy-peasy-devicetree-squeezy"
			["--debian", "sun7i-a20-cubietruck"]
			`assume` MadeChange
	dtb = "/etc/flash-kernel/dtbs/sun7i-a20-cubietruck.dtb"
	mydts =
		[ "/* Device tree addition enabling onewire sensors on CubieTruck GPIO pin PC21 */"
		, "#include <dt-bindings/gpio/gpio.h>"
		, ""
		, "/ {"
		, "\tonewire_device {"
		, "\t\tcompatible = \"w1-gpio\";"
		, "\t\tgpios = <&pio 2 21 GPIO_ACTIVE_HIGH>; /* PC21 */"
		, "\t\tpinctrl-names = \"default\";"
		, "\t\tpinctrl-0 = <&my_w1_pin>;"
		, "\t};"
		, "};"
		, ""
		, "&pio {"
		, "\tmy_w1_pin: my_w1_pin@0 {"
		, "\t\tallwinner,pins = \"PC21\";"
		, "\t\tallwinner,function = \"gpio_in\";"
		, "\t};"
		, "};"
		]

-- My home networked attached storage server.
homeNAS :: Property DebianLike
homeNAS = propertyList "home NAS" $ props
	& Apt.installed ["uhubctl"]
	& "/etc/udev/rules.d/52-startech-hub.rules" `File.hasContent`
		[ "# let users power control startech hub with uhubctl"
		, "ATTR{idVendor}==\"" ++ hubvendor ++ "\", ATTR{idProduct}==\"005a\", MODE=\"0666\""
		]
	& autoMountDrive "archive-10" (USBHubPort hubvendor 1) (Just "archive-older")
	& autoMountDrive "archive-11" (USBHubPort hubvendor 2) (Just "archive-old")
	& autoMountDrive "archive-12" (USBHubPort hubvendor 3) (Just "archive")
	& autoMountDrive "passport" (USBHubPort hubvendor 4) Nothing
	& Apt.installed ["git-annex", "borgbackup"]
  where
	hubvendor = "0409"

data USBHubPort = USBHubPort String Int

-- Makes a USB drive with the given label automount, and unmount after idle
-- for a while.
--
-- The hub port is turned on and off automatically as needed, using
-- uhubctl.
autoMountDrive :: Mount.Label -> USBHubPort -> Maybe FilePath -> Property DebianLike
autoMountDrive label (USBHubPort hubvendor port) malias = propertyList desc $ props
	& File.ownerGroup mountpoint (User "joey") (Group "joey")
		`requires` File.dirExists mountpoint
	& case malias of
		Just t -> ("/media/joey/" ++ t) `File.isSymlinkedTo`
			File.LinkTarget mountpoint
		Nothing -> doNothing <!> doNothing
	& File.hasContent ("/etc/systemd/system/" ++ mount)
		[ "[Unit]"
		, "Description=" ++ label
		, "Requires=" ++ hub
		, "After=" ++ hub
		, "[Mount]"
		-- avoid mounting whenever the block device is available,
		-- only want to automount on demand
		, "Options=noauto"
		, "What=" ++ devfile
		, "Where=" ++ mountpoint
		, "[Install]"
		, "WantedBy="
		]
		`onChange` Systemd.daemonReloaded
	& File.hasContent ("/etc/systemd/system/" ++ hub)
		[ "[Unit]"
		, "Description=Startech usb hub port " ++ show port
		, "PartOf=" ++ mount
		, "[Service]"
		, "Type=oneshot"
		, "RemainAfterExit=true"
		, "ExecStart=/usr/sbin/uhubctl -a on -p " ++ show port ++ " --vendor " ++ hubvendor 
		, "ExecStop=/bin/sh -c 'uhubctl -a off -p " ++ show port ++ " --vendor " ++ hubvendor
			-- Powering off the port does not remove device
			-- files, so ask udev to remove the devfile; it will
			-- be added back after the drive next spins up
			-- and so avoid mount happening before the drive is
			-- spun up.
			-- (This only works when the devfile is in
			-- by-label.)
			++ "; udevadm trigger --action=remove " ++ devfile ++ " || true'"
		, "[Install]"
		, "WantedBy="
		]
		`onChange` Systemd.daemonReloaded
	& File.hasContent ("/etc/systemd/system/" ++ automount)
		[ "[Unit]"
		, "Description=Automount " ++ label
		, "[Automount]"
		, "Where=" ++ mountpoint
		, "TimeoutIdleSec=300"
		, "[Install]"
		, "WantedBy=multi-user.target"
		]
		`onChange` Systemd.daemonReloaded
	& Systemd.enabled automount
	& Systemd.started automount
	& Sudo.sudoersDFile ("automount-" ++ label)
		[ "joey ALL= NOPASSWD: " ++ sudocommands
		]
  where
	devfile = "/dev/disk/by-label/" ++ label
	mountpoint = "/media/joey/" ++ label
	desc = "auto mount " ++ mountpoint
	hub = "startech-hub-port-" ++ show port ++ ".service"
	automount = svcbase ++ ".automount"
	mount = svcbase ++ ".mount"
	svcbase = Systemd.escapePath mountpoint
	sudocommands = intercalate " , " $ map (\c -> "/bin/systemctl " ++ c)
		[ "stop " ++ mountpoint
		, "start " ++ mountpoint
		]

rsyncNetBorgRepo :: String -> [Borg.BorgRepoOpt] -> Borg.BorgRepo
rsyncNetBorgRepo d os = Borg.BorgRepoUsing os' ("2318@usw-s002.rsync.net:" ++ d)
  where
	-- rsync.net has a newer borg here
	os' = Borg.UsesEnvVar ("BORG_REMOTE_PATH", "borg1") : os

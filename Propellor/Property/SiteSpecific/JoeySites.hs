-- | Specific configuation for Joey Hess's sites. Probably not useful to
-- others except as an example.

module Propellor.Property.SiteSpecific.JoeySites where

import Propellor
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.File as File
import qualified Propellor.Property.Gpg as Gpg
import qualified Propellor.Property.Ssh as Ssh
import qualified Propellor.Property.Git as Git
import qualified Propellor.Property.Cron as Cron
import qualified Propellor.Property.Service as Service
import qualified Propellor.Property.User as User
import qualified Propellor.Property.Obnam as Obnam
import qualified Propellor.Property.Apache as Apache
import Utility.SafeCommand
import Utility.FileMode

import Data.List
import System.Posix.Files

oldUseNetServer :: [Host] -> Property
oldUseNetServer hosts = propertyList ("olduse.net server")
	[ oldUseNetInstalled "oldusenet-server"
	, Obnam.latestVersion
	, Obnam.backup datadir "33 4 * * *"
		[ "--repository=sftp://2318@usw-s002.rsync.net/~/olduse.net"
		, "--client-name=spool"
		] Obnam.OnlyClient
		`requires` Ssh.keyImported SshRsa "root"
		`requires` Ssh.knownHost hosts "usw-s002.rsync.net" "root"
	, check (not . isSymbolicLink <$> getSymbolicLinkStatus newsspool) $
		property "olduse.net spool in place" $ makeChange $ do
			removeDirectoryRecursive newsspool
			createSymbolicLink (datadir </> "news") newsspool
	, Apt.installed ["leafnode"]
	, "/etc/news/leafnode/config" `File.hasContent` 
		[ "# olduse.net configuration (deployed by propellor)"
		, "expire = 1000000" -- no expiry via texpire
		, "server = " -- no upstream server
		, "debugmode = 1"
		, "allowSTRANGERS = 42" -- lets anyone connect
		, "nopost = 1" -- no new posting (just gather them)
		]
	, "/etc/hosts.deny" `File.lacksLine` "leafnode: ALL"
	, Apt.serviceInstalledRunning "openbsd-inetd"
	, File.notPresent "/etc/cron.daily/leafnode"
	, File.notPresent "/etc/cron.d/leafnode"
	, Cron.niceJob "oldusenet-expire" "11 1 * * *" "news" newsspool $ intercalate ";"
		[ "find \\( -path ./out.going -or -path ./interesting.groups -or -path './*/.overview' \\) -prune -or -type f -ctime +60  -print | xargs --no-run-if-empty rm"
		, "find -type d -empty | xargs --no-run-if-empty rmdir"
		]
	, Cron.niceJob "oldusenet-uucp" "*/5 * * * *" "news" "/" $
		"/usr/bin/uucp " ++ datadir
	, toProp $ Apache.siteEnabled "nntp.olduse.net" $ apachecfg "nntp.olduse.net" False
		[ "  DocumentRoot " ++ datadir ++ "/"
		, "  <Directory " ++ datadir ++ "/>"
		, "    Options Indexes FollowSymlinks"
		, "    AllowOverride None"
		-- I had this in the file before.
		-- This may be needed by a newer version of apache?
		--, "    Require all granted"
		, "  </Directory>"
		]
	]
  where
	newsspool = "/var/spool/news"
	datadir = "/var/spool/oldusenet"

oldUseNetShellBox :: Property
oldUseNetShellBox = oldUseNetInstalled "oldusenet"

oldUseNetInstalled :: Apt.Package -> Property
oldUseNetInstalled pkg = check (not <$> Apt.isInstalled pkg) $
	propertyList ("olduse.net " ++ pkg)
		[ Apt.installed (words "build-essential devscripts debhelper git libncursesw5-dev libpcre3-dev pkg-config bison libicu-dev libidn11-dev libcanlock2-dev libuu-dev ghc libghc-strptime-dev libghc-hamlet-dev libghc-ifelse-dev libghc-hxt-dev libghc-utf8-string-dev libghc-missingh-dev libghc-sha-dev")
			`describe` "olduse.net build deps"
		, scriptProperty
			[ "rm -rf /root/tmp/oldusenet" -- idenpotency
			, "git clone git://olduse.net/ /root/tmp/oldusenet/source"
			, "cd /root/tmp/oldusenet/source/"
			, "dpkg-buildpackage -us -uc"
			, "dpkg -i ../" ++ pkg ++ "_*.deb || true"
			, "apt-get -fy install" -- dependencies
			, "rm -rf /root/tmp/oldusenet"
			] `describe` "olduse.net built"
		]


kgbServer :: Property
kgbServer = withOS desc $ \o -> case o of
	(Just (System (Debian Unstable) _)) ->
		ensureProperty $ propertyList desc
			[ Apt.serviceInstalledRunning "kgb-bot"
			, File.hasPrivContent "/etc/kgb-bot/kgb.conf"
				`onChange` Service.restarted "kgb-bot"
			, "/etc/default/kgb-bot" `File.containsLine` "BOT_ENABLED=1"
				`describe` "kgb bot enabled"
				`onChange` Service.running "kgb-bot"
			]
	_ -> error "kgb server needs Debian unstable (for kgb-bot 1.31+)"
  where
	desc = "kgb.kitenet.net setup"

mumbleServer :: [Host] -> Property
mumbleServer hosts = combineProperties "mumble.debian.net" 
	[ Apt.serviceInstalledRunning "mumble-server"
	, Obnam.latestVersion
	, Obnam.backup "/var/lib/mumble-server" "55 5 * * *"
		[ "--repository=sftp://joey@turtle.kitenet.net/~/lib/backup/mumble.debian.net.obnam"
		, "--client-name=mumble"
		] Obnam.OnlyClient
		`requires` Ssh.keyImported SshRsa "root"
		`requires` Ssh.knownHost hosts "turtle.kitenet.net" "root"
	, trivial $ cmdProperty "chown" ["-R", "mumble-server:mumble-server", "/var/lib/mumble-server"]
	]

obnamLowMem :: Property
obnamLowMem = combineProperties "obnam tuned for low memory use"
	[ Obnam.latestVersion
	, "/etc/obnam.conf" `File.containsLines`
		[ "[config]"
		, "# Suggested by liw to keep Obnam memory consumption down (at some speed cost)."
		, "upload-queue-size = 128"
		, "lru-size = 128"
		]
	]

-- git.kitenet.net and git.joeyh.name
gitServer :: [Host] -> Property
gitServer hosts = propertyList "git.kitenet.net setup"
	[ Obnam.latestVersion
	, Obnam.backup "/srv/git" "33 3 * * *"
		[ "--repository=sftp://2318@usw-s002.rsync.net/~/git.kitenet.net"
		, "--encrypt-with=1B169BE1"
		, "--client-name=wren"
		] Obnam.OnlyClient
		`requires` Gpg.keyImported "1B169BE1" "root"
		`requires` Ssh.keyImported SshRsa "root"
		`requires` Ssh.knownHost hosts "usw-s002.rsync.net" "root"
		`requires` Ssh.authorizedKeys "family"
		`requires` User.accountFor "family"
	, Apt.installed ["git", "rsync", "kgb-client-git", "gitweb"]
	, Apt.installedBackport ["git-annex"]
	, File.hasPrivContentExposed "/etc/kgb-bot/kgb-client.conf"
	, toProp $ Git.daemonRunning "/srv/git"
	, "/etc/gitweb.conf" `File.containsLines`
		[ "$projectroot = '/srv/git';"
		, "@git_base_url_list = ('git://git.kitenet.net', 'http://git.kitenet.net/git', 'https://git.kitenet.net/git', 'ssh://git.kitenet.net/srv/git');"
		, "# disable snapshot download; overloads server"
		, "$feature{'snapshot'}{'default'} = [];"
		]
		`describe` "gitweb configured"
	-- Repos push on to github.
	, Ssh.knownHost hosts "github.com" "joey"
	-- I keep the website used for gitweb checked into git..
	, Git.cloned "root" "/srv/git/joey/git.kitenet.net.git" "/srv/web/git.kitenet.net" Nothing
	, website "git.kitenet.net"
	, website "git.joeyh.name"
	, toProp $ Apache.modEnabled "cgi"
	]
  where
	website hn = toProp $ Apache.siteEnabled hn $ apachecfg hn True
		[ "  DocumentRoot /srv/web/git.kitenet.net/"
		, "  <Directory /srv/web/git.kitenet.net/>"
		, "    Options Indexes ExecCGI FollowSymlinks"
		, "    AllowOverride None"
		, "    AddHandler cgi-script .cgi"
		, "    DirectoryIndex index.cgi"
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
annexWebSite :: [Host] -> Git.RepoUrl -> HostName -> AnnexUUID -> [(String, Git.RepoUrl)] -> Property
annexWebSite hosts origin hn uuid remotes = propertyList (hn ++" website using git-annex")
	[ Git.cloned "joey" origin dir Nothing
		`onChange` setup
	, postupdatehook `File.hasContent`
		[ "#!/bin/sh"
		, "exec git update-server-info"
		] `onChange`
			(postupdatehook `File.mode` (combineModes (ownerWriteMode:readModes ++ executeModes)))
	, setupapache
	]
  where
	dir = "/srv/web/" ++ hn
	postupdatehook = dir </> ".git/hooks/post-update"
	setup = userScriptProperty "joey" setupscript
		`requires` Ssh.keyImported SshRsa "joey"
		`requires` Ssh.knownHost hosts "turtle.kitenet.net" "joey"
	setupscript = 
		[ "cd " ++ shellEscape dir
		, "git config annex.uuid " ++ shellEscape uuid
		] ++ map addremote remotes ++
		[ "git annex get"
		]
	addremote (name, url) = "git remote add " ++ shellEscape name ++ " " ++ shellEscape url
	setupapache = toProp $ Apache.siteEnabled hn $ apachecfg hn True $ 
		[ "  ServerAlias www."++hn
		, ""
		, "  DocumentRoot /srv/web/"++hn
		, "  <Directory /srv/web/"++hn++">"
		, "    Options FollowSymLinks"
		, "    AllowOverride None"
		, "  </Directory>"
		, "  <Directory /srv/web/"++hn++">"
		, "    Options Indexes FollowSymLinks ExecCGI"
		, "    AllowOverride None"
		, "    AddHandler cgi-script .cgi"
		, "    DirectoryIndex index.html index.cgi"
		, "    Order allow,deny"
		, "    allow from all"
		, "  </Directory>"
		]

apachecfg :: HostName -> Bool -> Apache.ConfigFile -> Apache.ConfigFile
apachecfg hn withssl middle
	| withssl = vhost False ++ vhost True
	| otherwise = vhost False
  where
	vhost ssl = 
		[ "<VirtualHost *:"++show port++">"
		, "  ServerAdmin grue@joeyh.name"
		, "  ServerName "++hn++":"++show port
		]
		++ mainhttpscert ssl
		++ middle ++
		[ ""
		, "  ErrorLog /var/log/apache2/error.log"
		, "  LogLevel warn"
		, "  CustomLog /var/log/apache2/access.log combined"
		, "  ServerSignature On"
		, "  "
		, "  <Directory \"/usr/share/apache2/icons\">"
		, "      Options Indexes MultiViews"
		, "      AllowOverride None"
		, "      Order allow,deny"
		, "      Allow from all"
		, "  </Directory>"
		, "</VirtualHost>"
		]
	  where
		port = if ssl then 443 else 80 :: Int

mainhttpscert :: Bool -> Apache.ConfigFile
mainhttpscert False = []
mainhttpscert True = 
	[ "  SSLEngine on"
	, "  SSLCertificateFile /etc/ssl/certs/web.pem"
	, "  SSLCertificateKeyFile /etc/ssl/private/web.pem"
	, "  SSLCertificateChainFile /etc/ssl/certs/startssl.pem"
	]
		
gitAnnexDistributor :: Property
gitAnnexDistributor = combineProperties "git-annex distributor, including rsync server and signer"
	[ Apt.installed ["rsync"]
	, File.hasPrivContent "/etc/rsyncd.conf"
	, File.hasPrivContent "/etc/rsyncd.secrets"
	, "/etc/default/rsync" `File.containsLine` "RSYNC_ENABLE=true"
			`onChange` Service.running "rsync"
	, endpoint "/srv/web/downloads.kitenet.net/git-annex/autobuild"
	, endpoint "/srv/web/downloads.kitenet.net/git-annex/autobuild/x86_64-apple-mavericks"
	-- git-annex distribution signing key
	, Gpg.keyImported "89C809CB" "joey"
	]
  where
	endpoint d = combineProperties ("endpoint " ++ d)
		[ File.dirExists d
		, File.ownerGroup d "joey" "joey"
		]

-- Twitter, you kill us.
twitRss :: Property
twitRss = combineProperties "twitter rss"
	[ Git.cloned "joey" "git://git.kitenet.net/twitrss.git" dir Nothing
	, check (not <$> doesFileExist (dir </> "twitRss")) $
		userScriptProperty "joey"
			[ "cd " ++ dir
			, "ghc --make twitRss" 
			]
			`requires` Apt.installed
				[ "libghc-xml-dev"
				, "libghc-feed-dev"
				, "libghc-tagsoup-dev"
				]
	, feed "http://twitter.com/search/realtime?q=git-annex" "git-annex-twitter"
	, feed "http://twitter.com/search/realtime?q=olduse+OR+git-annex+OR+debhelper+OR+etckeeper+OR+ikiwiki+-ashley_ikiwiki" "twittergrep"
	]
  where
	dir = "/srv/web/tmp.kitenet.net/twitrss"
	crontime = "15 * * * *"
	feed url desc = Cron.job desc crontime "joey" dir $
		"./twitRss " ++ shellEscape url ++ " > " ++ shellEscape ("../" ++ desc ++ ".rss")

ircBouncer :: Property
ircBouncer = propertyList "IRC bouncer"
	[ Apt.installed ["znc"]
	, User.accountFor "znc"
	, File.hasPrivContent conf
	, File.ownerGroup conf "znc" "znc"
	, Cron.job "znconboot" "@reboot" "znc" "~" "znc"
	, Cron.job "zncrunning" "@hourly" "znc" "~" "znc || true"
	]
  where
	conf = "/home/znc/.znc/configs/znc.conf"

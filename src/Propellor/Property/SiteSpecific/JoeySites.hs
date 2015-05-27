-- | Specific configuration for Joey Hess's sites. Probably not useful to
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
import qualified Propellor.Property.Postfix as Postfix
import Utility.FileMode

import Data.List
import System.Posix.Files
import Data.String.Utils

scrollBox :: Property HasInfo
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
		, "echo 'or, type comments below (finish with a dot on its own line)'"
		, "echo"
		, "echo Your comments:"
		, "timeout --kill-after 1m --foreground 2h mail -E -s \"scroll test $t\" joey@kitenet.net"
		] `onChange` (g `File.mode` (combineModes (ownerWriteMode:readModes ++ executeModes)))
	& Apt.installed ["bsd-mailx"]
	-- prevent port forwarding etc by not letting scroll log in via ssh
	& Ssh.sshdConfig `File.containsLine` ("DenyUsers scroll")
		`onChange` Ssh.restarted
	& User.shellSetTo (User "scroll") s
	& User.hasPassword (User "scroll")
	& Apt.serviceInstalledRunning "telnetd"
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

oldUseNetServer :: [Host] -> Property HasInfo
oldUseNetServer hosts = propertyList "olduse.net server" $ props
	& Apt.installed ["leafnode"]
	& oldUseNetInstalled "oldusenet-server"
	& oldUseNetBackup
	& check (not . isSymbolicLink <$> getSymbolicLinkStatus newsspool)
		(property "olduse.net spool in place" $ makeChange $ do
			removeDirectoryRecursive newsspool
			createSymbolicLink (datadir </> "news") newsspool
		)
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
	nntpcfg = apachecfg "nntp.olduse.net" False
		[ "  DocumentRoot " ++ datadir ++ "/"
		, "  <Directory " ++ datadir ++ "/>"
		, "    Options Indexes FollowSymlinks"
		, "    AllowOverride None"
		, Apache.allowAll
		, "  </Directory>"
		]

	oldUseNetBackup = Obnam.backup datadir (Cron.Times "33 4 * * *")
		[ "--repository=sftp://2318@usw-s002.rsync.net/~/olduse.net"
		, "--client-name=spool"
		, "--ssh-key=" ++ keyfile
		] Obnam.OnlyClient
		`requires` Ssh.keyImported' (Just keyfile) SshRsa (User "root") (Context "olduse.net")
		`requires` Ssh.knownHost hosts "usw-s002.rsync.net" (User "root")
	keyfile = "/root/.ssh/olduse.net.key"

oldUseNetShellBox :: Property HasInfo
oldUseNetShellBox = propertyList "olduse.net shellbox" $ props
	& oldUseNetInstalled "oldusenet"
	& Service.running "shellinabox"

oldUseNetInstalled :: Apt.Package -> Property HasInfo
oldUseNetInstalled pkg = check (not <$> Apt.isInstalled pkg) $
	propertyList ("olduse.net " ++ pkg) $ props
		& Apt.installed (words "build-essential devscripts debhelper git libncursesw5-dev libpcre3-dev pkg-config bison libicu-dev libidn11-dev libcanlock2-dev libuu-dev ghc libghc-strptime-dev libghc-hamlet-dev libghc-ifelse-dev libghc-hxt-dev libghc-utf8-string-dev libghc-missingh-dev libghc-sha-dev")
			`describe` "olduse.net build deps"
		& scriptProperty
			[ "rm -rf /root/tmp/oldusenet" -- idenpotency
			, "git clone git://olduse.net/ /root/tmp/oldusenet/source"
			, "cd /root/tmp/oldusenet/source/"
			, "dpkg-buildpackage -us -uc"
			, "dpkg -i ../" ++ pkg ++ "_*.deb || true"
			, "apt-get -fy install" -- dependencies
			, "rm -rf /root/tmp/oldusenet"
			] `describe` "olduse.net built"

kgbServer :: Property HasInfo
kgbServer = propertyList desc $ props
	& installed
	& File.hasPrivContent "/etc/kgb-bot/kgb.conf" anyContext
		`onChange` Service.restarted "kgb-bot"
  where
	desc = "kgb.kitenet.net setup"
	installed = withOS desc $ \o -> case o of
		(Just (System (Debian Unstable) _)) ->
			ensureProperty $ propertyList desc
				[ Apt.serviceInstalledRunning "kgb-bot"
				, "/etc/default/kgb-bot" `File.containsLine` "BOT_ENABLED=1"
					`describe` "kgb bot enabled"
					`onChange` Service.running "kgb-bot"
				]
		_ -> error "kgb server needs Debian unstable (for kgb-bot 1.31+)"

mumbleServer :: [Host] -> Property HasInfo
mumbleServer hosts = combineProperties hn $ props
	& Apt.serviceInstalledRunning "mumble-server"
	& Obnam.backup "/var/lib/mumble-server" (Cron.Times "55 5 * * *")
		[ "--repository=sftp://2318@usw-s002.rsync.net/~/" ++ hn ++ ".obnam"
		, "--client-name=mumble"
		] Obnam.OnlyClient
		`requires` Ssh.keyImported SshRsa (User "root") (Context hn)
		`requires` Ssh.knownHost hosts "usw-s002.rsync.net" (User "root")
	& trivial (cmdProperty "chown" ["-R", "mumble-server:mumble-server", "/var/lib/mumble-server"])
  where
	hn = "mumble.debian.net"

-- git.kitenet.net and git.joeyh.name
gitServer :: [Host] -> Property HasInfo
gitServer hosts = propertyList "git.kitenet.net setup" $ props
	& Obnam.backupEncrypted "/srv/git" (Cron.Times "33 3 * * *")
		[ "--repository=sftp://2318@usw-s002.rsync.net/~/git.kitenet.net"
		, "--ssh-key=" ++ sshkey
		, "--client-name=wren" -- historical
		] Obnam.OnlyClient (Gpg.GpgKeyId "1B169BE1")
		`requires` Ssh.keyImported' (Just sshkey) SshRsa (User "root") (Context "git.kitenet.net")
		`requires` Ssh.knownHost hosts "usw-s002.rsync.net" (User "root")
		`requires` Ssh.authorizedKeys (User "family") (Context "git.kitenet.net")
		`requires` User.accountFor (User "family")
	& Apt.installed ["git", "rsync", "gitweb"]
	& Apt.installed ["git-annex"]
	& Apt.installed ["kgb-client"]
	& File.hasPrivContentExposed "/etc/kgb-bot/kgb-client.conf" anyContext
		`requires` File.dirExists "/etc/kgb-bot/"
	& Git.daemonRunning "/srv/git"
	& "/etc/gitweb.conf" `File.containsLines`
		[ "$projectroot = '/srv/git';"
		, "@git_base_url_list = ('git://git.kitenet.net', 'http://git.kitenet.net/git', 'https://git.kitenet.net/git', 'ssh://git.kitenet.net/srv/git');"
		, "# disable snapshot download; overloads server"
		, "$feature{'snapshot'}{'default'} = [];"
		]
		`describe` "gitweb configured"
	-- Repos push on to github.
	& Ssh.knownHost hosts "github.com" (User "joey")
	-- I keep the website used for gitweb checked into git..
	& Git.cloned (User "root") "/srv/git/joey/git.kitenet.net.git" "/srv/web/git.kitenet.net" Nothing
	& website "git.kitenet.net"
	& website "git.joeyh.name"
	& Apache.modEnabled "cgi"
  where
	sshkey = "/root/.ssh/git.kitenet.net.key"
	website hn = apacheSite hn True
		[ "  DocumentRoot /srv/web/git.kitenet.net/"
		, "  <Directory /srv/web/git.kitenet.net/>"
		, "    Options Indexes ExecCGI FollowSymlinks"
		, "    AllowOverride None"
		, "    AddHandler cgi-script .cgi"
		, "    DirectoryIndex index.cgi"
		, Apache.allowAll
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
annexWebSite :: Git.RepoUrl -> HostName -> AnnexUUID -> [(String, Git.RepoUrl)] -> Property HasInfo
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
	setupscript = 
		[ "cd " ++ shellEscape dir
		, "git annex reinit " ++ shellEscape uuid
		] ++ map addremote remotes ++
		[ "git annex get"
		, "git update-server-info"
		]
	addremote (name, url) = "git remote add " ++ shellEscape name ++ " " ++ shellEscape url
	setupapache = apacheSite hn True
		[ "  ServerAlias www."++hn
		, ""
		, "  DocumentRoot /srv/web/"++hn
		, "  <Directory /srv/web/"++hn++">"
		, "    Options FollowSymLinks"
		, "    AllowOverride None"
		, Apache.allowAll
		, "  </Directory>"
		, "  <Directory /srv/web/"++hn++">"
		, "    Options Indexes FollowSymLinks ExecCGI"
		, "    AllowOverride None"
		, "    AddHandler cgi-script .cgi"
		, "    DirectoryIndex index.html index.cgi"
		, Apache.allowAll
		, "  </Directory>"
		]

apacheSite :: HostName -> Bool -> Apache.ConfigFile -> RevertableProperty
apacheSite hn withssl middle = Apache.siteEnabled hn $ apachecfg hn withssl middle

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
		, Apache.allowAll
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
		
gitAnnexDistributor :: Property HasInfo
gitAnnexDistributor = combineProperties "git-annex distributor, including rsync server and signer" $ props
	& Apt.installed ["rsync"]
	& File.hasPrivContent "/etc/rsyncd.conf" (Context "git-annex distributor")
		`onChange` Service.restarted "rsync"
	& File.hasPrivContent "/etc/rsyncd.secrets" (Context "git-annex distributor")
		`onChange` Service.restarted "rsync"
	& "/etc/default/rsync" `File.containsLine` "RSYNC_ENABLE=true"
		`onChange` Service.running "rsync"
	& endpoint "/srv/web/downloads.kitenet.net/git-annex/autobuild"
	& endpoint "/srv/web/downloads.kitenet.net/git-annex/autobuild/x86_64-apple-yosemite"
	& endpoint "/srv/web/downloads.kitenet.net/git-annex/autobuild/windows"
	-- git-annex distribution signing key
	& Gpg.keyImported (Gpg.GpgKeyId "89C809CB") (User "joey")
  where
	endpoint d = combineProperties ("endpoint " ++ d)
		[ File.dirExists d
		, File.ownerGroup d (User "joey") (Group "joey")
		]

downloads :: [Host] -> Property HasInfo
downloads hosts = annexWebSite "/srv/git/downloads.git"
	"downloads.kitenet.net"
	"840760dc-08f0-11e2-8c61-576b7e66acfd"
	[("eubackup", "ssh://eubackup.kitenet.net/~/lib/downloads/")]
	`requires` Ssh.knownHost hosts "eubackup.kitenet.net" (User "joey")
	
tmp :: Property HasInfo
tmp = propertyList "tmp.kitenet.net" $ props
	& annexWebSite "/srv/git/joey/tmp.git"
		"tmp.kitenet.net"
		"26fd6e38-1226-11e2-a75f-ff007033bdba"
		[]
	& twitRss
	& pumpRss

-- Twitter, you kill us.
twitRss :: Property HasInfo
twitRss = combineProperties "twitter rss" $ props
	& Git.cloned (User "joey") "git://git.kitenet.net/twitrss.git" dir Nothing
	& check (not <$> doesFileExist (dir </> "twitRss")) compiled
	& feed "http://twitter.com/search/realtime?q=git-annex" "git-annex-twitter"
	& feed "http://twitter.com/search/realtime?q=olduse+OR+git-annex+OR+debhelper+OR+etckeeper+OR+ikiwiki+-ashley_ikiwiki" "twittergrep"
  where
	dir = "/srv/web/tmp.kitenet.net/twitrss"
	crontime = Cron.Times "15 * * * *"
	feed url desc = Cron.job desc crontime (User "joey") dir $
		"./twitRss " ++ shellEscape url ++ " > " ++ shellEscape ("../" ++ desc ++ ".rss")
	compiled = userScriptProperty (User "joey")
		[ "cd " ++ dir
		, "ghc --make twitRss" 
		]
		`requires` Apt.installed
			[ "libghc-xml-dev"
			, "libghc-feed-dev"
			, "libghc-tagsoup-dev"
			]

-- Work around for expired ssl cert.
pumpRss :: Property NoInfo
pumpRss = Cron.job "pump rss" (Cron.Times "15 * * * *") (User "joey") "/srv/web/tmp.kitenet.net/"
	"wget https://pump2rss.com/feed/joeyh@identi.ca.atom -O pump.atom.new --no-check-certificate 2>/dev/null; sed 's/ & / /g' pump.atom.new > pump.atom"

ircBouncer :: Property HasInfo
ircBouncer = propertyList "IRC bouncer" $ props
	& Apt.installed ["znc"]
	& User.accountFor (User "znc")
	& File.dirExists (takeDirectory conf)
	& File.hasPrivContent conf anyContext
	& File.ownerGroup conf (User "znc") (Group "znc")
	& Cron.job "znconboot" (Cron.Times "@reboot") (User "znc") "~" "znc"
	-- ensure running if it was not already
	& trivial (userScriptProperty (User "znc") ["znc || true"])
		`describe` "znc running"
  where
	conf = "/home/znc/.znc/configs/znc.conf"

kiteShellBox :: Property NoInfo
kiteShellBox = propertyList "kitenet.net shellinabox"
	[ Apt.installed ["openssl", "shellinabox"]
	, File.hasContent "/etc/default/shellinabox"
		[ "# Deployed by propellor"
		, "SHELLINABOX_DAEMON_START=1"
		, "SHELLINABOX_PORT=443"
		, "SHELLINABOX_ARGS=\"--no-beep --service=/:SSH:kitenet.net\""
		]
		`onChange` Service.restarted "shellinabox"
	, Service.running "shellinabox"
	]

githubBackup :: Property HasInfo
githubBackup = propertyList "github-backup box" $ props
	& Apt.installed ["github-backup", "moreutils"]
	& githubKeys
	& Cron.niceJob "github-backup run" (Cron.Times "30 4 * * *") (User "joey")
		"/home/joey/lib/backup" backupcmd
	& Cron.niceJob "gitriddance" (Cron.Times "30 4 * * *") (User "joey")
		"/home/joey/lib/backup" gitriddancecmd
  where
	backupcmd = intercalate "&&" $
		[ "mkdir -p github"
		, "cd github"
		, ". $HOME/.github-keys"
		, "github-backup joeyh"
		]
	gitriddancecmd = intercalate "&&" $
		[ "cd github"
		, ". $HOME/.github-keys"
		] ++ map gitriddance githubMirrors
	gitriddance (r, msg) = "(cd " ++ r ++ " && gitriddance " ++ shellEscape msg ++ ")"

githubKeys :: Property HasInfo
githubKeys = 
	let f = "/home/joey/.github-keys"
	in File.hasPrivContent f anyContext
		`onChange` File.ownerGroup f (User "joey") (Group "joey")


-- these repos are only mirrored on github, I don't want
-- all the proprietary features
githubMirrors :: [(String, String)]
githubMirrors =
	[ ("ikiwiki", plzuseurl "http://ikiwiki.info/todo/")
	, ("git-annex", plzuseurl "http://git-annex.branchable.com/todo/")
	, ("myrepos", plzuseurl "http://myrepos.branchable.com/todo/")
	, ("propellor", plzuseurl "http://propellor.branchable.com/todo/")
	, ("etckeeper", plzuseurl "http://etckeeper.branchable.com/todo/")
	]
  where
	plzuseurl u = "Please submit changes to " ++ u ++ " instead of using github pull requests, which are not part of my workflow. -- A robot acting on behalf of Joey Hess"

rsyncNetBackup :: [Host] -> Property NoInfo
rsyncNetBackup hosts = Cron.niceJob "rsync.net copied in daily" (Cron.Times "30 5 * * *")
	(User "joey") "/home/joey/lib/backup" "mkdir -p rsync.net && rsync --delete -az 2318@usw-s002.rsync.net: rsync.net"
	`requires` Ssh.knownHost hosts "usw-s002.rsync.net" (User "joey")

backupsBackedupFrom :: [Host] -> HostName -> FilePath -> Property NoInfo
backupsBackedupFrom hosts srchost destdir = Cron.niceJob desc
	(Cron.Times "@reboot") (User "joey") "/" cmd
	`requires` Ssh.knownHost hosts srchost (User "joey")
  where
	desc = "backups copied from " ++ srchost ++ " on boot"
	cmd = "sleep 30m && rsync -az --bwlimit=300K --partial --delete " ++ srchost ++ ":lib/backup/ " ++ destdir </> srchost

obnamRepos :: [String] -> Property NoInfo
obnamRepos rs = propertyList ("obnam repos for " ++ unwords rs)
	(mkbase : map mkrepo rs)
  where
	mkbase = mkdir "/home/joey/lib/backup"
		`requires` mkdir "/home/joey/lib"
	mkrepo r = mkdir ("/home/joey/lib/backup/" ++ r ++ ".obnam")
	mkdir d = File.dirExists d
		`before` File.ownerGroup d (User "joey") (Group "joey")

podcatcher :: Property NoInfo
podcatcher = Cron.niceJob "podcatcher run hourly" (Cron.Times "55 * * * *")
	(User "joey") "/home/joey/lib/sound/podcasts"
	"xargs git-annex importfeed -c annex.genmetadata=true < feeds; mr --quiet update"
	`requires` Apt.installed ["git-annex", "myrepos"]

kiteMailServer :: Property HasInfo
kiteMailServer = propertyList "kitenet.net mail server" $ props
	& Postfix.installed
	& Apt.installed ["postfix-pcre"]
	& Apt.serviceInstalledRunning "postgrey"

	& Apt.serviceInstalledRunning "spamassassin"
	& "/etc/default/spamassassin" `File.containsLines`
		[ "# Propellor deployed"
		, "ENABLED=1"
		, "OPTIONS=\"--create-prefs --max-children 5 --helper-home-dir\""
		, "CRON=1"
		, "NICE=\"--nicelevel 15\""
		] `onChange` Service.restarted "spamassassin"
		`describe` "spamd enabled"
		`requires` Apt.serviceInstalledRunning "cron"
	
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

	& dkimInstalled

	& Postfix.saslAuthdInstalled

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
	& hasJoeyCAChain
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

		, "# Password auth for relaying (used by errol)"
		, "smtpd_sasl_auth_enable = yes"
		, "smtpd_sasl_security_options = noanonymous"
		, "smtpd_sasl_local_domain = kitenet.net"

		, "# Enable postgrey."
		, "smtpd_recipient_restrictions = permit_tls_clientcerts,permit_sasl_authenticated,,permit_mynetworks,reject_unauth_destination,check_policy_service inet:127.0.0.1:10023"

		, "# Enable spamass-milter, amavis-milter, opendkim"
		, "smtpd_milters = unix:/spamass/spamass.sock unix:amavis/amavis.sock inet:localhost:8891"
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
		, "pass=$HOME/.pine-password"
		, "if [ ! -e $pass ]; then"
		, "\ttouch $pass"
		, "fi"
		, "chmod 600 $pass"
		, "exec alpine -passfile $pass \"$@\""
		]
		`onChange` (pinescript `File.mode`
			combineModes (readModes ++ executeModes))
		`describe` "pine wrapper script"
	& "/etc/pine.conf" `File.hasContent`
		[ "# deployed with propellor"
		, "inbox-path={localhost/novalidate-cert/NoRsh}inbox"
		]
		`describe` "pine configured to use local imap server"
	
	& Apt.serviceInstalledRunning "mailman"
  where
	ctx = Context "kitenet.net"
	pinescript = "/usr/local/bin/pine"
	dovecotusers = "/etc/dovecot/users"

-- Configures postfix to relay outgoing mail to kitenet.net, with
-- verification via tls cert.
postfixClientRelay :: Context -> Property HasInfo
postfixClientRelay ctx = Postfix.mainCfFile `File.containsLines`
	[ "relayhost = kitenet.net"
	, "smtp_tls_CAfile = /etc/ssl/certs/joeyca.pem"
	, "smtp_tls_cert_file = /etc/ssl/certs/postfix.pem"
	, "smtp_tls_key_file = /etc/ssl/private/postfix.pem"
	, "smtp_tls_loglevel = 0"
	, "smtp_use_tls = yes"
	]
	`describe` "postfix client relay"
	`onChange` Postfix.dedupMainCf
	`onChange` Postfix.reloaded
	`requires` hasJoeyCAChain
	`requires` hasPostfixCert ctx

-- Configures postfix to have the dkim milter, and no other milters.
dkimMilter :: Property HasInfo
dkimMilter = Postfix.mainCfFile `File.containsLines`
	[ "smtpd_milters = inet:localhost:8891"
	, "non_smtpd_milters = inet:localhost:8891"
	, "milter_default_action = accept"
	]
	`describe` "postfix dkim milter"
	`onChange` Postfix.dedupMainCf
	`onChange` Postfix.reloaded
	`requires` dkimInstalled

-- This does not configure postfix to use the dkim milter,
-- nor does it set up domainkey DNS.
dkimInstalled :: Property HasInfo
dkimInstalled = go `onChange` Service.restarted "opendkim"
  where
	go = propertyList "opendkim installed" $ props
		& Apt.serviceInstalledRunning "opendkim"
		& File.dirExists "/etc/mail"
		& File.hasPrivContent "/etc/mail/dkim.key" (Context "kitenet.net")
		& File.ownerGroup "/etc/mail/dkim.key" (User "opendkim") (Group "opendkim")
		& "/etc/default/opendkim" `File.containsLine`
			"SOCKET=\"inet:8891@localhost\""
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
domainKey = (RelDomain "mail._domainkey", TXT "v=DKIM1; k=rsa; t=y; p=MIGfMA0GCSqGSIb3DQEBAQUAA4GNADCBiQKBgQCc+/rfzNdt5DseBBmfB3C6sVM7FgVvf4h1FeCfyfwPpVcmPdW6M2I+NtJsbRkNbEICxiP6QY2UM0uoo9TmPqLgiCCG2vtuiG6XMsS0Y/gGwqKM7ntg/7vT1Go9vcquOFFuLa5PnzpVf8hB9+PMFdS4NPTvWL2c5xxshl/RJzICnQIDAQAB")

hasJoeyCAChain :: Property HasInfo
hasJoeyCAChain = "/etc/ssl/certs/joeyca.pem" `File.hasPrivContentExposed`
	Context "joeyca.pem"

hasPostfixCert :: Context -> Property HasInfo
hasPostfixCert ctx = combineProperties "postfix tls cert installed"
	[ "/etc/ssl/certs/postfix.pem" `File.hasPrivContentExposed` ctx
	, "/etc/ssl/private/postfix.pem" `File.hasPrivContent` ctx
	]

kitenetHttps :: Property HasInfo
kitenetHttps = propertyList "kitenet.net https certs" $ props
	& File.hasPrivContent "/etc/ssl/certs/web.pem" ctx
	& File.hasPrivContent "/etc/ssl/private/web.pem" ctx
	& File.hasPrivContent "/etc/ssl/certs/startssl.pem" ctx
	& Apache.modEnabled "ssl"
  where
	ctx = Context "kitenet.net"

-- Legacy static web sites and redirections from kitenet.net to newer
-- sites.
legacyWebSites :: Property HasInfo
legacyWebSites = propertyList "legacy web sites" $ props
	& Apt.serviceInstalledRunning "apache2"
	& Apache.modEnabled "rewrite"
	& Apache.modEnabled "cgi"
	& Apache.modEnabled "speling"
	& userDirHtml
	& kitenetHttps
	& apacheSite "kitenet.net" True
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
		, "RewriteRule /simpleid/ http://openid.kitenet.net:8081/simpleid/"
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
	& alias "anna.kitenet.net"
	& apacheSite "anna.kitenet.net" False
		[ "DocumentRoot /home/anna/html"
		, "<Directory /home/anna/html/>"
		, "  Options Indexes ExecCGI"
		, "  AllowOverride None"
		, Apache.allowAll
		, "</Directory>"
		]
	& alias "sows-ear.kitenet.net"
	& alias "www.sows-ear.kitenet.net"
	& apacheSite "sows-ear.kitenet.net" False
		[ "ServerAlias www.sows-ear.kitenet.net"
		, "DocumentRoot /srv/web/sows-ear.kitenet.net"
		, "<Directory /srv/web/sows-ear.kitenet.net>"
		, "  Options FollowSymLinks"
		, "  AllowOverride None"
		, Apache.allowAll
		, "</Directory>"
		]
	& alias "wortroot.kitenet.net"
	& alias "www.wortroot.kitenet.net"
	& apacheSite "wortroot.kitenet.net" False
		[ "ServerAlias www.wortroot.kitenet.net"
		, "DocumentRoot /srv/web/wortroot.kitenet.net"
		, "<Directory /srv/web/wortroot.kitenet.net>"
		, "  Options FollowSymLinks"
		, "  AllowOverride None"
		, Apache.allowAll
		, "</Directory>"
		]
	& alias "creeksidepress.com"
	& apacheSite "creeksidepress.com" False
		[ "ServerAlias www.creeksidepress.com"
		, "DocumentRoot /srv/web/www.creeksidepress.com"
		, "<Directory /srv/web/www.creeksidepress.com>"
		, "  Options FollowSymLinks"
		, "  AllowOverride None"
		, Apache.allowAll
		, "</Directory>"
		]
	& alias "joey.kitenet.net"
	& apacheSite "joey.kitenet.net" False
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

userDirHtml :: Property HasInfo
userDirHtml = File.fileProperty "apache userdir is html" (map munge) conf
	`onChange` Apache.reloaded
	`requires` (toProp $ Apache.modEnabled "userdir")
  where
	munge = replace "public_html" "html"
	conf = "/etc/apache2/mods-available/userdir.conf"


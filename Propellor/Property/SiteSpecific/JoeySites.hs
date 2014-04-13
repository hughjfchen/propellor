-- | Specific configuation for Joey Hess's sites. Probably not useful to
-- others except as an example.

module Propellor.Property.SiteSpecific.JoeySites where

import Propellor
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.File as File
import qualified Propellor.Property.Gpg as Gpg
import qualified Propellor.Property.Ssh as Ssh
import qualified Propellor.Property.Git as Git
import qualified Propellor.Property.Service as Service
import qualified Propellor.Property.User as User
import qualified Propellor.Property.Obnam as Obnam
import qualified Propellor.Property.Apache as Apache

oldUseNetShellBox :: Property
oldUseNetShellBox = check (not <$> Apt.isInstalled "oldusenet") $
	propertyList ("olduse.net shellbox")
		[ Apt.installed (words "build-essential devscripts debhelper git libncursesw5-dev libpcre3-dev pkg-config bison libicu-dev libidn11-dev libcanlock2-dev libuu-dev ghc libghc-strptime-dev libghc-hamlet-dev libghc-ifelse-dev libghc-hxt-dev libghc-utf8-string-dev libghc-missingh-dev libghc-sha-dev")
			`describe` "olduse.net build deps"
		, scriptProperty
			[ "rm -rf /root/tmp/oldusenet" -- idenpotency
			, "git clone git://olduse.net/ /root/tmp/oldusenet/source"
			, "cd /root/tmp/oldusenet/source/"
			, "dpkg-buildpackage -us -uc"
			, "dpkg -i ../oldusenet*.deb || true"
			, "apt-get -fy install" -- dependencies
			, "rm -rf /root/tmp/oldusenet"
			] `describe` "olduse.net built"
		]

-- git.kitenet.net and git.joeyh.name
gitServer :: [Host] -> Property
gitServer hosts = propertyList "git.kitenet.net setup"
	[ Obnam.backup "/srv/git" "33 3 * * *"
		[ "--repository=sftp://2318@usw-s002.rsync.net/~/git.kitenet.net"
		, "--encrypt-with=1B169BE1"
		, "--client-name=wren"
		] Obnam.OnlyClient
		`requires` Gpg.keyImported "1B169BE1" "root"
		`requires` Ssh.keyImported SshRsa "root"
		`requires` Ssh.knownHost hosts "usw-s002.rsync.net" "root"
		`requires` Ssh.authorizedKeys "family"
		`requires` User.accountFor "family"
	, Apt.installed ["git", "git-annex", "rsync", "kgb-client-git", "gitweb"]
	, File.hasPrivContentExposed "/etc/kgb-bot/kgb-client.conf"
	, toProp $ Git.daemonRunning "/srv/git"
	, "/etc/gitweb.conf" `File.containsLines`
		[ "$projectroot = '/srv/git';"
		, "@git_base_url_list = ('git://git.kitenet.net', 'http://git.kitenet.net/git', 'ssh://git.kitenet.net/srv/git');"
		, "# disable snapshot download; overloads server"
		, "$feature{'snapshot'}{'default'} = [];"
		]
		`describe` "gitweb configured"
	-- I keep the website used for gitweb checked into git..
	, Git.cloned "root" "/srv/git/joey/git.kitenet.net.git" "/srv/web/git.kitenet.net" Nothing
	, website "git.kitenet.net"
	, website "git.joeyh.name"
	, toProp $ Apache.modEnabled "cgi"
	-- TODO: upgrade to newer git-annex-shell for notification
	]
  where
	website hn = toProp $ Apache.siteEnabled hn (gitapacheconf hn)

gitapacheconf :: HostName -> Apache.ConfigFile
gitapacheconf hn =
	[ "<VirtualHost *:80>"
	, "  ServerAdmin joey@kitenet.net"
	, ""
	, "  ServerName " ++ hn ++ ":80"
	, ""
	, "  DocumentRoot /srv/web/git.kitenet.net/"
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
	, ""
	, "  ErrorLog /var/log/apache2/error.log"
	, "  LogLevel warn"
	, "  CustomLog /var/log/apache2/access.log combined"
	, ""
	, "  # Possible values include: debug, info, notice, warn, error, crit,"
	, "  # alert, emerg."
	, "  LogLevel warn"
	, ""
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

-- Note: needs debian unstable for new kgb
kgbServer :: Property
kgbServer = propertyList "kgb.kitenet.net setup"
	[ Apt.serviceInstalledRunning "kgb-bot"
	, File.hasPrivContent "/etc/kgb-bot/kgb.conf"
		`onChange` Service.restarted "kgb-bot"
	, "/etc/default/kgb-bot" `File.containsLine` "BOT_ENABLED=1"
		`describe` "kgb bot enabled"
		`onChange` Service.running "kgb-bot"
	]


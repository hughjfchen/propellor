module Propellor.Property.SiteSpecific.Branchable where

import Propellor.Base
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.File as File
import qualified Propellor.Property.User as User
import qualified Propellor.Property.Ssh as Ssh
import qualified Propellor.Property.Postfix as Postfix
import qualified Propellor.Property.Sudo as Sudo
import qualified Propellor.Property.Borg as Borg
import qualified Propellor.Property.Cron as Cron
import Propellor.Property.SiteSpecific.JoeySites (rsyncNetBorgRepo)

server :: [Host] -> Property (HasInfo + DebianLike)
server hosts = propertyList "branchable server" $ props
	& "/etc/timezone" `File.hasContent` ["Etc/UTC"]
	& "/etc/locale.gen" `File.containsLines`
		[ "en_GB.UTF-8 UTF-8"
		, "en_US.UTF-8 UTF-8"
		, "fi_FI.UTF-8 UTF-8"
		]
		`onChange` (cmdProperty "locale-gen" [] `assume` MadeChange)

	& Apt.installed ["etckeeper", "ssh", "popularity-contest"]
	& Apt.serviceInstalledRunning "apache2"
	& Apt.serviceInstalledRunning "ntp"

	& Apt.serviceInstalledRunning "openssh-server"
	& Ssh.passwordAuthentication False
	& Ssh.hostKeys (Context "branchable.com")
		[ (SshEcdsa, "ecdsa-sha2-nistp256 AAAAE2VjZHNhLXNoYTItbmlzdHAyNTYAAAAIbmlzdHAyNTYAAABBBLRRZ3Ew6eq6d8+ID1CXwF0hLjObNM2XwCIOFI4Wml2iP5NIHwtUCg2hlVUal6v1bO+VPjvx3dkf5Y00GI2BVSY= root@pell")
		, (SshEd25519, "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIG5gaqToi0NtZH+vxXqW8E/reJW2tMHOEs2ycieMYkng root@pell")
		]

	& Apt.installed ["procmail", "bsd-mailx"]
	& "/etc/aliases" `File.hasPrivContentExposed` (Context "branchable.com")
		`onChange` Postfix.newaliases
	& "/etc/mailname" `File.hasContent` ["branchable.com"]
	& Postfix.installed
	& Postfix.mainCf ("mailbox_command", "procmail -a \"$EXTENSION\"")
	
	-- backup everything except the contents of sites, which are
	-- backed up by ikiwiki-hosting.
	& Borg.backup "/" (rsyncNetBorgRepo "pell.borg" []) Cron.Daily
		[ "--exclude=/proc/*"
	 	, "--exclude=/sys/*"
	 	, "--exclude=/run/*"
	 	, "--exclude=/tmp/*"
	 	, "--exclude=/var/tmp/*"
	 	, "--exclude=/var/backups/ikiwiki-hosting-web/*"
	 	, "--exclude=/var/cache/*"
	 	, "--exclude=/home/*/source/*"
	 	, "--exclude=/home/*/source.git/*"
	 	, "--exclude=/home/*/public_html/*"
	 	, "--exclude=/home/*/.git/*"
	 	]
	 	[ Borg.KeepDays 7
	 	, Borg.KeepWeeks 5
	 	, Borg.KeepMonths 3
	 	, Borg.KeepYears 1
	 	]
	& Ssh.userKeys (User "root") (Context "branchable.com")
		[ (SshEd25519, "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIC20PCLAgkD6tK0vYsl0Pdpho+y7fNay8Fo8SXWrZojs root@pell")
		]
	& Ssh.knownHost hosts "usw-s002.rsync.net" (User "root")

	& adminuser "joey"
	& adminuser "liw"
  where
	adminuser u = propertyList ("admin user " ++ u) $ props
		& User.accountFor (User u)
		& User.hasSomePassword (User u)
		& Sudo.enabledFor (User u)
		& User.hasGroup (User u) (Group "adm")
		& User.hasGroup (User u) (Group "systemd-journal")

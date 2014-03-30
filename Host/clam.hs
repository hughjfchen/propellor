import Property
import qualified Property.Apt as Apt
import qualified Property.Ssh as Ssh
import qualified Property.User as User
import qualified Property.Hostname as Hostname
import qualified Property.GitHome as GitHome
import qualified Property.Reboot as Reboot

main = defaultMain
	[ Apt.stdSourcesList Apt.Unstable `onChange` Apt.upgrade
	, Apt.installed ["etckeeper"]
	, Hostname.set "clam.kitenet.net"
	, Apt.installed ["ssh"]
	, Ssh.uniqueHostKeys
	, Apt.installed ["git", "myrepos"]
	, GitHome.installedFor "root"
	, check (Ssh.hasAuthorizedKeys "root") $
		Ssh.passwordAuthentication False
	, User.nonsystem "joey"
	, Apt.installed ["sudo"]
	, fileHasContent "/etc/sudoers" ["joey ALL=(ALL:ALL) ALL"]
	, GitHome.installedFor "joey"
	, Apt.removed ["exim4"] `onChange` Apt.autoRemove
	, Apt.installed ["tor"]
	, Apt.installed ["systemd-sysv"] `onChange` Reboot.scheduled "+10"
	]

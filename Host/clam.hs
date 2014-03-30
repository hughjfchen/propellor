import Property
import qualified Property.Apt as Apt
import qualified Property.Ssh as Ssh
import qualified Property.User as User
import qualified Property.GitHome as GitHome

main = defaultMain
	[ Apt.stdSourcesList Apt.Unstable `onChange` Apt.upgrade
	, Apt.installed ["etckeeper"]
	, Apt.installed ["ssh"]
	, Apt.installed ["git", "myrepos"]
	, GitHome.installed "root"
	, check (Ssh.hasAuthorizedKeys "root") $
		Ssh.passwordAuthentication False
	, User.nonsystem "joey"
	, fileHasContent "/etc/sudoers" ["joey ALL=(ALL:ALL) ALL"]
	, GitHome.installed "joey"
	, Apt.removed ["exim4"] `onChange` Apt.autoRemove
	, Apt.installed ["tor"]
	]

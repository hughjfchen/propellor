import Property
import HostName
import qualified Property.Apt as Apt
import qualified Property.Ssh as Ssh
import qualified Property.User as User
import qualified Property.Hostname as Hostname
import qualified Property.GitHome as GitHome
import qualified Property.Reboot as Reboot

main :: IO ()
main = ensureProperties . getProperties =<< getHostName

{- This is where the system's HostName, either as returned by uname
 - or one specified on the command line is converted into a list of
 - Properties for that system. -}
getProperties :: HostName -> [Property]
getProperties "clam" =
	[ Apt.stdSourcesList Apt.Unstable `onChange` Apt.upgrade
	, Apt.installed ["etckeeper"]
	, Hostname.set "clam.kitenet.net"
	, Apt.installed ["ssh"]
	, Ssh.uniqueHostKeys
	, Apt.installed ["git", "myrepos"]
	, GitHome.installedFor "root"
	, check (Ssh.hasAuthorizedKeys "root") $
		Ssh.passwordAuthentication False
	, check (Ssh.hasAuthorizedKeys "root") $
		User.lockedPassword "root"
	, User.nonsystem "joey"
	, User.nuked "user"
	, Apt.installed ["sudo"]
	, lineInfFile "/etc/sudoers" "joey ALL=(ALL:ALL) ALL"
	, GitHome.installedFor "joey"
	, Apt.removed ["exim4"] `onChange` Apt.autoRemove
	, Apt.installed ["tor"]
	, Apt.installed ["systemd-sysv"] `onChange` Reboot.scheduled "+10"
	]
-- add more hosts here...
--getProperties "foo" =
getProperties h = error $ "Unknown host: " ++ h ++ " (perhaps you should specify the real hostname on the command line?)"

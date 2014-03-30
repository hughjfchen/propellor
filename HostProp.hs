import Property
import HostName
import qualified Property.Apt as Apt
import qualified Property.Ssh as Ssh
import qualified Property.User as User
import qualified Property.Hostname as Hostname
import qualified Property.Reboot as Reboot
import qualified Property.Tor as Tor
import qualified Property.GitHome as GitHome

main :: IO ()
main = ensureProperties . getProperties =<< getHostName

{- This is where the system's HostName, either as returned by uname
 - or one specified on the command line is converted into a list of
 - Properties for that system. -}
getProperties :: HostName -> [Property]
getProperties "clam.kitenet.net" =
	-- Clean up the system as installed by cloudatcost.com
	[ User.nuked "user"
	, Apt.removed ["exim4"] `onChange` Apt.autoRemove
	, Hostname.set "clam.kitenet.net"
	, Ssh.uniqueHostKeys
	-- This is my standard system setup
	, Apt.stdSourcesList Apt.Unstable `onChange` Apt.upgrade
	, Apt.installed ["etckeeper"]
	, Apt.installed ["ssh"]
	, GitHome.installedFor "root"
	-- Harden the system, but only once root's authorized_keys
	-- is safely in place.
	, check (Ssh.hasAuthorizedKeys "root") $
		Ssh.passwordAuthentication False
	, check (Ssh.hasAuthorizedKeys "root") $
		User.lockedPassword "root"
	, User.nonsystem "joey"
	, Apt.installed ["sudo"]
	, lineInFile "/etc/sudoers" "joey ALL=(ALL:ALL) ALL"
	, GitHome.installedFor "joey"
	-- Clam is a tor bridge.
	, Tor.isBridge
	-- Should come last as it reboots.
	, Apt.installed ["systemd-sysv"] `onChange` Reboot.scheduled "+10"
	]
-- add more hosts here...
--getProperties "foo" =
getProperties h = error $ "Unknown host: " ++ h ++ " (perhaps you should specify the real hostname on the command line?)"

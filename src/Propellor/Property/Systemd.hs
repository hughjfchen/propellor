{-# LANGUAGE FlexibleInstances #-}

module Propellor.Property.Systemd (
	-- * Services
	ServiceName,
	started,
	stopped,
	enabled,
	disabled,
	masked,
	running,
	restarted,
	networkd,
	journald,
	-- * Configuration
	installed,
	Option,
	configured,
	daemonReloaded,
	-- * Journal
	persistentJournal,
	journaldConfigured,
	-- * Containers and machined
	machined,
	MachineName,
	Container,
	container,
	nspawned,
	-- * Container configuration
	containerCfg,
	resolvConfed,
	linkJournal,
	privateNetwork,
	module Propellor.Types.Container,
	Proto(..),
	Publishable,
	publish,
	Bindable,
	bind,
	bindRo,
) where

import Propellor.Base
import Propellor.Types.Chroot
import Propellor.Types.Container
import Propellor.Types.Info
import qualified Propellor.Property.Chroot as Chroot
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.File as File
import Propellor.Property.Systemd.Core
import Utility.FileMode

import Data.List
import Data.List.Utils
import qualified Data.Map as M

type ServiceName = String

type MachineName = String

data Container = Container MachineName Chroot.Chroot Host
	deriving (Show)

instance PropAccum Container where
	(Container n c h) `addProp` p = Container n c (h `addProp` p)
	(Container n c h) `addPropFront` p = Container n c (h `addPropFront` p)
	getProperties (Container _ _ h) = hostProperties h

-- | Starts a systemd service.
--
-- Note that this does not configure systemd to start the service on boot,
-- it only ensures that the service is currently running.
started :: ServiceName -> Property NoInfo
started n = cmdProperty "systemctl" ["start", n]
	`assume` NoChange
	`describe` ("service " ++ n ++ " started")

-- | Stops a systemd service.
stopped :: ServiceName -> Property NoInfo
stopped n = cmdProperty "systemctl" ["stop", n]
	`assume` NoChange
	`describe` ("service " ++ n ++ " stopped")

-- | Enables a systemd service.
--
-- This does not ensure the service is started, it only configures systemd
-- to start it on boot.
enabled :: ServiceName -> Property NoInfo
enabled n = cmdProperty "systemctl" ["enable", n]
	`assume` NoChange
	`describe` ("service " ++ n ++ " enabled")

-- | Disables a systemd service.
disabled :: ServiceName -> Property NoInfo
disabled n = cmdProperty "systemctl" ["disable", n]
	`assume` NoChange
	`describe` ("service " ++ n ++ " disabled")

-- | Masks a systemd service.
masked :: ServiceName -> RevertableProperty NoInfo
masked n = systemdMask <!> systemdUnmask
  where
	systemdMask = cmdProperty "systemctl" ["mask", n]
		`assume` NoChange
		`describe` ("service " ++ n ++ " masked")
	systemdUnmask = cmdProperty "systemctl" ["unmask", n]
		`assume` NoChange
		`describe` ("service " ++ n ++ " unmasked")

-- | Ensures that a service is both enabled and started
running :: ServiceName -> Property NoInfo
running n = started n `requires` enabled n

-- | Restarts a systemd service.
restarted :: ServiceName -> Property NoInfo
restarted n = cmdProperty "systemctl" ["restart", n]
	`assume` NoChange
	`describe` ("service " ++ n ++ " restarted")

-- | The systemd-networkd service.
networkd :: ServiceName
networkd = "systemd-networkd"

-- | The systemd-journald service.
journald :: ServiceName
journald = "systemd-journald"

-- | Enables persistent storage of the journal.
persistentJournal :: Property NoInfo
persistentJournal = check (not <$> doesDirectoryExist dir) $
	combineProperties "persistent systemd journal"
		[ cmdProperty "install" ["-d", "-g", "systemd-journal", dir]
			`assume` MadeChange
		, cmdProperty "setfacl" ["-R", "-nm", "g:adm:rx,d:g:adm:rx", dir]
			`assume` MadeChange
		, started "systemd-journal-flush"
		]
		`requires` Apt.installed ["acl"]
  where
	dir = "/var/log/journal"

type Option = String

-- | Ensures that an option is configured in one of systemd's config files.
-- Does not ensure that the relevant daemon notices the change immediately.
--
-- This assumes that there is only one [Header] per file, which is
-- currently the case for files like journald.conf and system.conf.
-- And it assumes the file already exists with
-- the right [Header], so new lines can just be appended to the end.
configured :: FilePath -> Option -> String -> Property NoInfo
configured cfgfile option value = combineProperties desc
	[ File.fileProperty desc (mapMaybe removeother) cfgfile
	, File.containsLine cfgfile line
	]
  where
	setting = option ++ "="
	line = setting ++ value
	desc = cfgfile ++ " " ++ line
	removeother l
		| setting `isPrefixOf` l && l /= line = Nothing
		| otherwise = Just l

-- | Causes systemd to reload its configuration files.
daemonReloaded :: Property NoInfo
daemonReloaded = cmdProperty "systemctl" ["daemon-reload"]
	`assume` NoChange

-- | Configures journald, restarting it so the changes take effect.
journaldConfigured :: Option -> String -> Property NoInfo
journaldConfigured option value =
	configured "/etc/systemd/journald.conf" option value
		`onChange` restarted journald

-- | Ensures machined and machinectl are installed
machined :: Property NoInfo
machined = withOS "machined installed" $ \o ->
	case o of
		-- Split into separate debian package since systemd 225.
		(Just (System (Debian suite) _))
			| not (isStable suite) -> ensureProperty $
				Apt.installed ["systemd-container"]
		_ -> noChange

-- | Defines a container with a given machine name, and operating system,
-- and how to create its chroot if not already present.
--
-- Properties can be added to configure the Container.
--
-- > container "webserver" (System (Debian Unstable) "amd64") (Chroot.debootstrapped mempty)
-- >    & Apt.installedRunning "apache2"
-- >    & ...
container :: MachineName -> System -> (FilePath -> Chroot.Chroot) -> Container
container name system mkchroot = Container name c h
	& os system
	& resolvConfed
	& linkJournal
  where
	c = mkchroot (containerDir name)
		& os system
	h = Host name [] mempty

-- | Runs a container using systemd-nspawn.
--
-- A systemd unit is set up for the container, so it will automatically
-- be started on boot.
--
-- Systemd is automatically installed inside the container, and will
-- communicate with the host's systemd. This allows systemctl to be used to
-- examine the status of services running inside the container.
--
-- When the host system has persistentJournal enabled, journactl can be
-- used to examine logs forwarded from the container.
--
-- Reverting this property stops the container, removes the systemd unit,
-- and deletes the chroot and all its contents.
nspawned :: Container -> RevertableProperty HasInfo
nspawned c@(Container name (Chroot.Chroot loc builder _) h) =
	p `describe` ("nspawned " ++ name)
  where
	p = enterScript c
		`before` chrootprovisioned
		`before` nspawnService c (_chrootCfg $ getInfo $ hostInfo h)
		`before` containerprovisioned

	-- Chroot provisioning is run in systemd-only mode,
	-- which sets up the chroot and ensures systemd and dbus are
	-- installed, but does not handle the other properties.
	chrootprovisioned = Chroot.provisioned' (Chroot.propagateChrootInfo chroot) chroot True

	-- Use nsenter to enter container and and run propellor to
	-- finish provisioning.
	containerprovisioned =
		Chroot.propellChroot chroot (enterContainerProcess c) False
			<!>
		doNothing

	chroot = Chroot.Chroot loc builder h

-- | Sets up the service file for the container, and then starts
-- it running.
nspawnService :: Container -> ChrootCfg -> RevertableProperty NoInfo
nspawnService (Container name _ _) cfg = setup <!> teardown
  where
	service = nspawnServiceName name
	servicefile = "/etc/systemd/system/multi-user.target.wants" </> service

	servicefilecontent = do
		ls <- lines <$> readFile "/lib/systemd/system/systemd-nspawn@.service"
		return $ unlines $
			"# deployed by propellor" : map addparams ls
	addparams l
		| "ExecStart=" `isPrefixOf` l = unwords $
			[ "ExecStart = /usr/bin/systemd-nspawn"
			, "--quiet"
			, "--keep-unit"
			, "--boot"
			, "--directory=" ++ containerDir name
			, "--machine=%i"
			] ++ nspawnServiceParams cfg
		| otherwise = l

	goodservicefile = (==)
		<$> servicefilecontent
		<*> catchDefaultIO "" (readFile servicefile)

	writeservicefile = property servicefile $ makeChange $ do
		c <- servicefilecontent
		File.viaStableTmp (\t -> writeFile t c) servicefile

	setupservicefile = check (not <$> goodservicefile) $
		-- if it's running, it has the wrong configuration,
		-- so stop it
		stopped service
			`requires` daemonReloaded
			`requires` writeservicefile

	setup = started service `requires` setupservicefile `requires` machined

	teardown = check (doesFileExist servicefile) $
		disabled service `requires` stopped service

nspawnServiceParams :: ChrootCfg -> [String]
nspawnServiceParams NoChrootCfg = []
nspawnServiceParams (SystemdNspawnCfg ps) =
	M.keys $ M.filter id $ M.fromList ps

-- | Installs a "enter-machinename" script that root can use to run a
-- command inside the container.
--
-- This uses nsenter to enter the container, by looking up the pid of the
-- container's init process and using its namespace.
enterScript :: Container -> RevertableProperty NoInfo
enterScript c@(Container name _ _) = setup <!> teardown
  where
	setup = combineProperties ("generated " ++ enterScriptFile c)
		[ scriptfile `File.hasContent`
			[ "#!/usr/bin/perl"
			, "# Generated by propellor"
			, "my $pid=`machinectl show " ++ shellEscape name ++ " -p Leader | cut -d= -f2`;"
			, "chomp $pid;"
			, "if (length $pid) {"
			, "\tforeach my $var (keys %ENV) {"
			, "\t\tdelete $ENV{$var} unless $var eq 'PATH' || $var eq 'TERM';"
			, "\t}"
			, "\texec('nsenter', '-p', '-u', '-n', '-i', '-m', '-t', $pid, @ARGV);"
			, "} else {"
			, "\tdie 'container not running';"
			, "}"
			, "exit(1);"
			]
		, scriptfile `File.mode` combineModes (readModes ++ executeModes)
		]
	teardown = File.notPresent scriptfile
	scriptfile = enterScriptFile c

enterScriptFile :: Container -> FilePath
enterScriptFile (Container name _ _ ) = "/usr/local/bin/enter-" ++ mungename name

enterContainerProcess :: Container -> [String] -> IO (CreateProcess, IO ())
enterContainerProcess c ps = pure (proc (enterScriptFile c) ps, noop)

nspawnServiceName :: MachineName -> ServiceName
nspawnServiceName name = "systemd-nspawn@" ++ name ++ ".service"

containerDir :: MachineName -> FilePath
containerDir name = "/var/lib/container" </> mungename name

mungename :: MachineName -> String
mungename = replace "/" "_"

-- | This configures how systemd-nspawn(1) starts the container,
-- by specifying a parameter, such as "--private-network", or
-- "--link-journal=guest"
--
-- When there is no leading dash, "--" is prepended to the parameter.
--
-- Reverting the property will remove a parameter, if it's present.
containerCfg :: String -> RevertableProperty HasInfo
containerCfg p = RevertableProperty (mk True) (mk False)
  where
	mk b = pureInfoProperty ("container configuration " ++ (if b then "" else "without ") ++ p') $
		mempty { _chrootCfg = SystemdNspawnCfg [(p', b)] }
	p' = case p of
		('-':_) -> p
		_ -> "--" ++ p

-- | Bind mounts </etc/resolv.conf> from the host into the container.
--
-- This property is enabled by default. Revert it to disable it.
resolvConfed :: RevertableProperty HasInfo
resolvConfed = containerCfg "bind=/etc/resolv.conf"

-- | Link the container's journal to the host's if possible.
-- (Only works if the host has persistent journal enabled.)
--
-- This property is enabled by default. Revert it to disable it.
linkJournal :: RevertableProperty HasInfo
linkJournal = containerCfg "link-journal=try-guest"

-- | Disconnect networking of the container from the host.
privateNetwork :: RevertableProperty HasInfo
privateNetwork = containerCfg "private-network"

class Publishable a where
	toPublish :: a -> String

instance Publishable Port where
	toPublish port = fromPort port

instance Publishable (Bound Port) where
	toPublish v = toPublish (hostSide v) ++ ":" ++ toPublish (containerSide v)

data Proto = TCP | UDP

instance Publishable (Proto, Bound Port) where
	toPublish (TCP, fp) = "tcp:" ++ toPublish fp
	toPublish (UDP, fp) = "udp:" ++ toPublish fp

-- | Publish a port from the container to the host.
--
-- This feature was first added in systemd version 220.
--
-- This property is only needed (and will only work) if the container
-- is configured to use private networking. Also, networkd should be enabled
-- both inside the container, and on the host. For example:
--
-- > foo :: Host
-- > foo = host "foo.example.com"
-- >	& Systemd.nspawned webserver
-- > 		`requires` Systemd.running Systemd.networkd
-- >
-- > webserver :: Systemd.container
-- > webserver = Systemd.container "webserver" (Chroot.debootstrapped mempty)
-- >	& os (System (Debian Testing) "amd64")
-- >	& Systemd.privateNetwork
-- >	& Systemd.running Systemd.networkd
-- >	& Systemd.publish (Port 80 ->- Port 8080)
-- >	& Apt.installedRunning "apache2"
publish :: Publishable p => p -> RevertableProperty HasInfo
publish p = containerCfg $ "--port=" ++ toPublish p

class Bindable a where
	toBind :: a -> String

instance Bindable FilePath where
	toBind f = f

instance Bindable (Bound FilePath) where
	toBind v = hostSide v ++ ":" ++ containerSide v

-- | Bind mount a file or directory from the host into the container.
bind :: Bindable p => p -> RevertableProperty HasInfo
bind p = containerCfg $ "--bind=" ++ toBind p

-- | Read-only mind mount.
bindRo :: Bindable p => p -> RevertableProperty HasInfo
bindRo p = containerCfg $ "--bind-ro=" ++ toBind p

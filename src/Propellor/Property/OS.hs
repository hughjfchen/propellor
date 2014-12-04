module Propellor.Property.OS (
	cleanInstallOnce,
	Confirmation(..),
	preserveNetworkInterfaces,
	preserveRootSshAuthorized,
	grubBoots,
	GrubDev(..),
	oldOSKernelPreserved,
	kernelInstalled,
	oldOSRemoved,
) where

import Propellor
import qualified Propellor.Property.Debootstrap as Debootstrap
import qualified Propellor.Property.Ssh as Ssh
import qualified Propellor.Property.User as User
import Propellor.Property.Mount

import System.Posix.Files (rename, fileExist)

-- | Replaces whatever OS was installed before with a clean installation
-- of the OS that the Host is configured to have.
--
-- This can replace one Linux distribution with different one.
-- But, it can also fail and leave the system in an unbootable state.
--
-- The files from the old os will be left in /old-os
--
-- To avoid this property being accidentially used, you have to provide
-- a Confirmation containing the name of the host that you intend to apply the
-- property to.
--
-- This property only runs once. The cleanly installed system will have
-- a file /etc/propellor-cleaninstall, which indicates it was cleanly
-- installed.
--
-- You will typically want to run some more properties after the clean
-- install, to bootstrap from the cleanly installed system to a fully
-- working system. For example:
--
-- > & os (System (Debian Unstable) "amd64")
-- > & cleanInstall (Confirmed "foo.example.com")
-- >    `onChange` propertyList "fixing up after clean install"
-- >        [ preserveNetworkInterfaces
-- >        , preserverRootSshAuthorized
-- >        , oldOSKernelPreserved
-- >        -- , kernelInstalled
-- >        -- , grubBoots "hd0"
-- >        -- , oldOsRemoved
-- >        ]
-- > & Apt.installed ["ssh"]
-- > & User.hasSomePassword "root"
-- > & User.accountFor "joey"
-- > & User.hasSomePassword "joey"
-- > -- rest of system properties here
cleanInstallOnce :: Confirmation -> Property
cleanInstallOnce confirmation = check (not <$> doesFileExist flagfile) $
	go `requires` confirmed "clean install confirmed" confirmation
  where
	go = 
		finalized
			`requires`
		propellorbootstrapped
			`requires`
		User.shadowConfig True
			`requires`
		flipped
			`requires`
		umountall
			`requires`
		osbootstrapped

	osbootstrapped = withOS (newOSDir ++ " bootstrapped") $ \o -> case o of
		(Just d@(System (Debian _) _)) -> debootstrap d
		(Just u@(System (Ubuntu _) _)) -> debootstrap u
		_ -> error "os is not declared to be Debian or Ubuntu"
	debootstrap targetos = ensureProperty $ toProp $
		Debootstrap.built newOSDir targetos Debootstrap.DefaultConfig
	
	umountall = property "mount points unmounted" $ liftIO $ do
		mnts <- filter (`notElem` ("/": trickydirs)) <$> mountPoints
		-- reverse so that deeper mount points come first
		forM_ (reverse mnts) umountLazy
		return $ if null mnts then NoChange else MadeChange

	flipped = property (newOSDir ++ " moved into place") $ liftIO $ do
		createDirectoryIfMissing True oldOSDir
		rootcontents <- dirContents "/"
		forM_ rootcontents $ \d ->
			when (d `notElem` (oldOSDir:newOSDir:trickydirs)) $
				rename d (oldOSDir ++ d)
		newrootcontents <- dirContents newOSDir
		forM_ newrootcontents $ \d -> do
			let dest = "/" ++ takeFileName d
			whenM (not <$> fileExist dest) $
				rename d dest
		removeDirectoryRecursive newOSDir
		return MadeChange
	
	trickydirs = 
		-- /tmp can contain X's sockets, which prevent moving it
		-- so it's left as-is.
		[ "/tmp"
		-- /proc is left mounted
		, "/proc"
		]

	propellorbootstrapped = property "propellor re-debootstrapped in new os" $
		return NoChange
		-- re-bootstrap propellor in /usr/local/propellor,
		--   (using git repo bundle, privdata file, and possibly
		--   git repo url, which all need to be arranged to
		--   be present in /old-os's /usr/local/propellor)
	
	finalized = property "clean OS installed" $ do
		liftIO $ writeFile flagfile ""
		return MadeChange

	flagfile = "/etc/propellor-cleaninstall"

data Confirmation = Confirmed HostName

confirmed :: Desc -> Confirmation -> Property
confirmed desc (Confirmed c) = property desc $ do
	hostname <- asks hostName
	if hostname /= c
		then do
			warningMessage "Run with a bad confirmation, not matching hostname."
			return FailedChange
		else return NoChange

-- /etc/network/interfaces is configured to bring up all interfaces that
-- are currently up, using the same IP addresses.
preserveNetworkInterfaces :: Property
preserveNetworkInterfaces = undefined

-- Root's .ssh/authorized_keys has added to it any ssh keys that
-- were authorized in the old OS. Any other contents of the file are
-- retained.
preserveRootSshAuthorized :: Property
preserveRootSshAuthorized = check (doesDirectoryExist oldloc) $
	property (newloc ++ " copied from old OS") $ do
		ks <- liftIO $ lines <$> readFile oldloc
		ensureProperties (map (Ssh.authorizedKey "root") ks)
  where
	newloc = "/root/.ssh/authorized_keys"
	oldloc = oldOSDir ++ newloc

-- Installs an appropriate kernel from the OS distribution.
kernelInstalled :: Property
kernelInstalled = undefined

-- Copies kernel images, initrds, and modules from /old-os
-- into the new system.
--
-- TODO: grub config?
oldOSKernelPreserved :: Property
oldOSKernelPreserved = undefined

-- Installs grub onto a device to boot the system.
--
-- You may want to install grub to multiple devices; eg for a system
-- that uses software RAID.
grubBoots :: GrubDev -> Property
grubBoots = undefined

type GrubDev = String

-- Removes the old OS's backup from /old-os
oldOSRemoved :: Confirmation -> Property
oldOSRemoved confirmation = check (doesDirectoryExist oldOSDir) $
	go `requires` confirmed "old OS backup removal confirmed" confirmation
  where
	go = property "old OS backup removed" $ do
		liftIO $ removeDirectoryRecursive oldOSDir
		return MadeChange

oldOSDir :: FilePath
oldOSDir = "/old-os"

newOSDir :: FilePath
newOSDir = "/new-os"

{-# OPTIONS_HADDOCK prune #-}

{-|
Maintainer: Sean Whitton <spwhitton@spwhitton.name>

Build and maintain schroots for use with sbuild.

Suggested usage in @config.hs@:

>  & Sbuild.builtFor ((Debian Unstable) "i386")
>  & Sbuild.updatedFor ((Debian Unstable) "i386") `period` Weekly
>  & Sbuild.usableBy (User "spwhitton")
>  & Sbuild.shareAptCache
>  & Sbuild.blockNetwork
>  & Schroot.overlaysInTmpfs

In @~/.sbuildrc@:

>  $run_piuparts = 1;
>  $piuparts_opts = ['--schroot=unstable-i386-sbuild'];
>
>  $external_commands = {
>    'post-build-commands' => [
>      [
>        'adt-run',
>        '--changes', '%c',
>        '---',
>        'schroot', 'unstable-i386-sbuild',
>      ],
>    ],
>  };

We use @sbuild-createchroot(1)@ to create a chroot to the specification of
@sbuild-setup(7)@.  This differs from the approach taken by picca's Sbuild.hs,
which uses 'Propellor.Property.Debootstrap' to construct the chroot.  This is
because we don't want to run propellor inside the chroot in order to keep the
sbuild environment as standard as possible.
-}

-- If you wanted to do it with Propellor.Property.Debootstrap, note that
-- sbuild-createchroot has a --setup-only option

module Propellor.Property.Sbuild (
	-- * Creating and updating sbuild schroots
	SbuildSchroot(..),
	builtFor,
	built,
	updated,
	updatedFor,
	-- * Global sbuild configuration
	installed,
	keypairGenerated,
	shareAptCache,
	usableBy,
) where

import Propellor.Base
import Debootstrap (extractSuite)
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.Ccache as Ccache
import qualified Propellor.Property.File as File
import qualified Propellor.Property.Firewall as Firewall

import System.Directory
import System.FilePath (takeDirectory)

-- | An sbuild schroot, such as would be listed by @schroot -l@
--
-- Parts of the sbuild toolchain cannot distinguish between schroots with both
-- the same suite and the same architecture, so neither do we
data SbuildSchroot = SbuildSchroot Suite Architecture

-- | Build and configure a schroot for use with sbuild using a distribution's
-- standard mirror
--
-- This function is a convenience wrapper around 'Sbuild.builtFor', allowing the
-- user to identify the schroot and distribution using the 'System' type
builtFor :: System -> Property DebianLike
builtFor system = case schrootFromSystem system of
	Just s  -> built s (stdMirror system)
	Nothing -> errorMessage "don't know how to debootstrap " ++ show system

-- TODO should be revertable (and that should carry through to builtFor)
-- | Build and configure a schroot for use with sbuild
built :: SbuildSchroot -> Apt.Url -> Property DebianLike
built s@(SbuildSchroot suite arch) mirror = check (not <$> doesDirectoryExist (schrootRoot s)) $
	property ("built schroot for " ++ show s) go
	`requires` keypairGenerated
	`requires` ccachePrepared
	`requires` installed
  where
	go :: Property DebianLike
	go = do
		de <- standardPathEnv
		let params = Param <$>
			[ "--arch=" ++ arch
			, "--chroot-suffix=propellor"
			, "--include=eatmydata,ccache"
			, schrootRoot s
			, mirror
			]
		ifM (boolSystemEnv "sbuild-createchroot" params (Just de))
			( do
				fixConfFile s
				-- if we just built a sid chroot, add useful aliases
				when (suite == "unstable") $ ensureProperty $
					File.containsLine (schrootConf s)
					"aliases=UNRELEASED,sid,rc-buggy,experimental"
				-- enable ccache and eatmydata for speed
				ensureProperty $ File.containsLine (schrootConf s)
					"command-prefix=/var/cache/ccache-sbuild/sbuild-setup,eatmydata"
				return MadeChange
			, return FailedChange
			)

-- | Ensure that an sbuild schroot's packages and apt indexes are updated
--
-- This function is a convenience wrapper around 'Sbuild.updated', allowing the
-- user to identify the schroot using the 'System' type
updatedFor :: System -> Property DebianLike
updatedFor system = case schrootFromSystem system of
	Just s  -> updated s
	Nothing -> errorMessage "don't know how to debootstrap " ++ show system

-- | Ensure that an sbuild schroot's packages and apt indexes are updated
updated :: SbuildSchroot -> Property DebianLike
updated s@(SbuildSchroot suite arch) =
	check (doesDirectoryExist (schrootRoot s)) $
	property ("updated schroot for " ++ show s) go
	`requires` keypairGenerated
	`requires` installed
  where
	go :: Property DebianLike
	go = tightenTargets $ cmdProperty
		"sbuild-update" ["-udr", suite ++ "-" ++ arch]

-- Find the conf file that sbuild-createchroot(1) made when we passed it
-- --chroot-suffix=propellor, and edit and rename such that it is as if we
-- passed --chroot-suffix=sbuild (the default).  Replace the random suffix with
-- 'propellor'.
--
-- We had to pass --chroot-suffix=propellor in order that we can find a unique
-- config file for the schroot we just built, despite the random suffix.
--
-- The properties in this module only permit the creation of one chroot for a
-- given suite and architecture, so we don't need the suffix to be random.
fixConfFile :: SbuildSchroot -> IO ()
fixConfFile s@(SbuildSchroot suite arch) = do
	old <- take 1 . filter (tempPrefix `isPrefixOf`) <$> dirContents dir
	ensureProperty $ File.fileProperty "replace dummy suffix" (map munge) old
	moveFile old new
  where
	new = schrootConf s
	dir = takeDirectory new
	tempPrefix = dir </> suite ++ "-" ++ arch ++ "-propellor-"
	munge = replace "-propellor]" "-sbuild]"

-- | Bind-mount @/var/cache/apt/archives@ in all sbuild chroots so that the host
-- system and the chroot share the apt cache
--
-- This speeds up builds by avoiding unnecessary downloads of build
-- dependencies.
shareAptCache :: Property DebianLike
shareAptCache = File.containsLine "/etc/schroot/sbuild/fstab"
	"/var/cache/apt/archives /var/cache/apt/archives none rw,bind 0 0"
	`requires` installed

-- | Ensure that sbuild is installed
installed :: Property DebianLike
installed = Apt.installed ["sbuild"]

-- | Add an user to the sbuild group in order to use sbuild
usableBy :: User -> Property DebianLike
usableBy u = User.hasGroup u (Group "sbuild") `requires` installed

-- | Generate the apt keys needed by sbuild
keypairGenerated :: Property DebianLike
keypairGenerated = check (not <$> doesFileExist secKeyFile) $ go
	`requires` installed
  where
	go :: Property DebianLike
	go = tightenTargets $
		cmdProperty "sbuild-update" ["--keygen"]
		`assume` MadeChange
	secKeyFile = "/var/lib/sbuild/apt-keys/sbuild-key.sec"

-- another script from wiki.d.o/sbuild
ccachePrepared :: Property DebianLike
ccachePrepared = propertyList "sbuild group ccache configured" $ props
	& (Group "sbuild") `Ccache.hasGroupCache` "2G"
	& "/etc/schroot/sbuild/fstab" `File.containsLine`
		"/var/cache/ccache-sbuild /var/cache/ccache-sbuild rw,bind 0 0"
	& "/var/cache/ccache-sbuild/sbuild-setup" `File.hasContent`
		[ "#!/bin/sh"
		, "export CCACHE_DIR=/var/cache/ccache-sbuild"
		, "export CCACHE_UMASK=002"
		, "export CCACHE_COMPRESS=1"
		, "unset CCACHE_HARDLINK"
		, "export PATH=\"/usr/lib/ccache:$PATH\""
		, ""
		, "exec \"$@\""
		]
	& File.mode "/var/cache/ccache-sbuild/sbuild-setup"
		(combineModes (readModes ++ executeModes))

-- | Block network access during builds
--
-- This is a hack from <https://wiki.debian.org/sbuild> until #802850 and
-- #802849 are resolved.
blockNetwork :: Property Linux
blockNetwork = Firewall.rule OUTPUT Filter DROP
	(  GroupOwner (Group "sbuild")
	++ NotDestination [IPWithNumMask "127.0.0.1" "8"]
	)

-- ==== utility functions ====

schrootFromSystem :: System -> Maybe SbuildSchroot
schrootFromSystem system@(System _ arch) =
	extractSuite system
	>>= \suite -> return $ SbuildSchroot suite arch

stdMirror :: System -> Apt.Url
stdMirror (System (Debian _) _) = "http://httpredir.debian.org/debian"
stdMirror (System (Buntish _) _) = "TODO"

schrootRoot :: SbuildSchroot -> FilePath
schrootRoot (SbuildSchroot s a) = "/srv/chroot" </> s ++ "-" ++ a

schrootConf :: SbuildSchroot -> FilePath
schrootConf (SbuildSchroot s a) =
	"/etc/schroot/chroot.d" </> s ++ "-" ++ a ++ "-sbuild-propellor"

{-# OPTIONS_HADDOCK prune #-}

{-|
Maintainer: Sean Whitton <spwhitton@spwhitton.name>

Build and maintain schroots for use with sbuild.

Suggested usage in @config.hs@:

>  & Sbuild.builtFor ((Debian Unstable) "i386")
>  & Sbuild.updatedFor ((Debian Unstable) "i386") `period` Weekly
>  & Sbuild.usableBy (User "spwhitton")
>  & Sbuild.shareAptCache
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
import qualified Propellor.Property.File as File

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

-- | Build and configure a schroot for use with sbuild
built :: SbuildSchroot -> Apt.Url -> Property DebianLike
built s mirror = check (not <$> doesDirectoryExist (schrootRoot s)) go
	`requires` keypairGenerated
	`requires` installed

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
updated s = check (doesDirectoryExist (schrootRoot s)) go
	`requires` keypairGenerated
	`requires` installed

-- built' :: System -> Property DebianLike
-- built' system@(System distro arch) =
-- 	property' ("built chroot for " ++ show system) (liftIO go)
-- 	`requires` keypairGenerated
--   where
-- 	go = do
-- 		suite <- case extractSuite system of
-- 			Just s  -> return s
-- 			Nothing -> errorMessage $
-- 				"don't know how to debootstrap " ++ show system
-- 		de <- standardPathEnv
-- 		let params = Param <$>
-- 			[ "--arch=" ++ arch
-- 			, "--chroot-suffix=propellor"
-- 			, "--include=eatmydata,ccache"
-- 			, schrootLocation suite arch
-- 			, stdMirror distro
-- 			]
-- 		ifM (boolSystemEnv "sbuild-createchroot" params (Just de))
-- 			( do
-- 				fixConfFile suite arch
-- 				return MadeChange
-- 			, return FailedChange
-- 			)


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

-- -- | Update a schroot's installed packages and apt indexes.
-- updated :: System -> Property DebianLike
-- updated system@(System distro arch) = go `requires` installed
-- 	where
-- 		go :: Property DebianLike
-- 		go = tightenTargets $ cmdProperty
-- 			"sbuild-update" ["-udr", suite ++ "-" ++ arch]
-- 		suite = fromJust $ extractSuite system

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

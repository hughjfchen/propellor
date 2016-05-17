{-|
Maintainer: Sean Whitton <spwhitton@spwhitton.name>

Build and maintain schroots for use with sbuild.

Suggested usage in @config.hs@:

>  & Sbuild.built ((Debian Unstable) "i386")
>  & Sbuild.updated ((Debian Unstable) "i386") `period` Weekly
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
sbuild environment as standardised as possible.
-}

-- If you wanted to do it with Propellor.Property.Debootstrap, note that
-- sbuild-createchroot has a --setup-only option

-- TODO export useful properties only
module Propellor.Property.Sbuild where

import Propellor.Base
import Debootstrap (extractSuite)
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.File as File

import System.Directory

schrootChrootD :: FilePath
schrootChrootD = "/etc/schroot/chroot.d"

-- | Build and configure a schroot for use with sbuild
built :: System -> Property DebianLike
built system@(System distro arch) =
	property' ("built chroot for " ++ show system) (liftIO go)
	`requires` keypairGenerated
  where
	go = do
		suite <- case extractSuite system of
			Just s  -> return s
			Nothing -> errorMessage $
				"don't know how to debootstrap " ++ show system
		de <- standardPathEnv
		let params = Param <$>
			[ "--arch=" ++ arch
			-- We pass --chroot-suffix in order that we can find the
			-- config file despite the random suffix that
			-- sbuild-createchroot gives it.  We'll change this back
			-- to 'sbuild' once debootstrap has finished.
			, "--chroot-suffix=propellor"
			, "--include=eatmydata,ccache"
			, "/srv/chroot/" ++ suite ++ "-" ++ arch
			, stdMirror distro
			]
		ifM (boolSystemEnv "sbuild-createchroot" params (Just de))
			( do
				fixConfFile suite arch
				return MadeChange
			, return FailedChange
			)

fixConfFile :: String -> Architecture -> IO ()
fixConfFile suite arch = do
	confs <- dirContents schrootChrootD
	let conf = filter (schrootChrootD
		</> suite ++ "-" ++ arch ++ "-propellor-" `isPrefixOf`)
		confs
	ensureProperty $ File.fileProperty "replace dummy suffix" (map munge) conf
	moveFile conf $
		schrootChrootD </> suite ++ "-" ++ arch ++ "-sbuild-propellor"
  where
	munge = replace "-propellor]" "-sbuild]"

stdMirror :: System -> Apt.Url
stdMirror (System (Debian s) _) = "http://httpredir.debian.org/debian"
stdMirror (System (Buntish r) _) = "TODO"

-- | Update a schroot's installed packages and apt indexes.
updated :: System -> Property DebianLike
updated system@(System distro arch) = go `requires` installed
	where
		go :: Property DebianLike
		go = tightenTargets $ cmdProperty
 			"sbuild-update" ["-udr", suite ++ "-" ++ "arch"]
		suite = fromJust $ extractSuite system
-- TODO autoclean/clean only if shareAptCache property not present

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
keypairGenerated =
	check (not <$> doesFileExist secKeyFile) $ go
	`requires` installed
  where
	go :: Property DebianLike
	go = tightenTargets $
		cmdProperty "sbuild-update" ["--keygen"] `assume` MadeChange
	secKeyFile = "/var/lib/sbuild/apt-keys/sbuild-key.sec"

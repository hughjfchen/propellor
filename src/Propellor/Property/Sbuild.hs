{-|
Maintainer: Sean Whitton <spwhitton@spwhitton.name>

Build and maintain schroots for use with sbuild.

Suggested usage in @config.hs@:

>  & Sbuild.built (Debian Unstable) "i386"
>  & Sbuild.updated (Debian Unstable) "i386" `period` Weekly
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
which uses 'Propellor.Property.Debootstrap' to construct the chroot.  This is to
ensure that the clean package build environment is standardised.
-}

module Propellor.Property.Sbuild where

import Propellor.Base
import qualified Propellor.Property.Apt as Apt

-- | Update a schroot's installed packages and apt indexes.
updated :: System -> Architecture -> Property DebianLike
updated = undefined
-- TODO autoclean/clean only if shareAptCache property not present

-- | Bind-mount @/var/cache/apt/archives@ in all sbuild chroots so that the host
-- system and the chroot share the apt cache.
--
-- This speeds up builds by avoiding unnecessary downloads of build
-- dependencies.
shareAptCache :: Property DebianLike
shareAptCache = undefined

installed :: Property DebianLike
installed = Apt.installed ["sbuild"]

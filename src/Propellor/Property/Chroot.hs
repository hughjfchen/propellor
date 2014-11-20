module Propellor.Property.Chroot (
	Chroot,
	chroot,
	provisioned,
) where

import Propellor
import qualified Propellor.Property.Debootstrap as Debootstrap

import qualified Data.Map as M

data Chroot = Chroot FilePath System Host

instance Hostlike Chroot where
	(Chroot l s h) & p = Chroot l s (h & p)
	(Chroot l s h) &^ p = Chroot l s (h &^ p)
	getHost (Chroot _ _ h) = h

-- | Defines a Chroot at the given location, containing the specified
-- System. Properties can be added to configure the Chroot.
--
-- > chroot "/srv/chroot/ghc-dev" (System (Debian Unstable) "amd64"
-- >    & Apt.installed ["build-essential", "ghc", "haskell-platform"]
-- >	& ...
chroot :: FilePath -> System -> Chroot
chroot location system = Chroot location system (Host location [] mempty)

-- | Ensures that the chroot exists and is provisioned according to its
-- properties.
--
-- Reverting this property removes the chroot. Note that it does not ensure
-- that any processes that might be running inside the chroot are stopped.
provisioned :: Chroot -> RevertableProperty
provisioned c@(Chroot loc system _) = RevertableProperty
	(propigateChrootInfo c (go "exists" setup))
	(go "removed" teardown)
  where
	go desc a = property ("chroot " ++ loc ++ " " ++ desc) $ do
		ensureProperties [a]

	setup = provisionChroot c `requires` built
	
	built = case system of
		(System (Debian _) _) -> debootstrap
		(System (Ubuntu _) _) -> debootstrap

	debootstrap = unrevertable (Debootstrap.built loc system [])

	teardown = undefined

propigateChrootInfo :: Chroot -> Property -> Property
propigateChrootInfo c@(Chroot loc _ h) p = propigateInfo c p (<> chrootinfo)
  where
	chrootinfo = mempty $ mempty { _chroots = M.singleton loc h }

provisionChroot :: Chroot -> Property
provisionChroot = undefined

-- | This module provides properties for Buntish.
module Propellor.Property.AptSoftwarePropertiesCommon where

import Data.List
import Control.Applicative
import Prelude
import Data.String.Utils
import Data.String (IsString(..))
import Propellor.Base
import qualified Propellor.Property.Apt as Apt

-- | Ensure it's installed in case it's not. It's part of Buntish's defaults so
-- one might assume...
installed :: Property NoInfo
installed = Apt.installed ["software-properties-common"]

-- | Personal Package Archives are people's individual package contributions to
-- Ubuntu. There's a well-known format for adding them, and this type represents
-- that. It's also an instance of 'Show' and 'IsString' so it can work with
-- 'OverloadedStrings'. More on PPAs can be found at
-- <https://help.launchpad.net/Packaging/PPA>
data PPA = PPA {
        -- | The Launchpad account hosting this archive.
        ppaAccount :: String,
        -- | The
        ppaArchive :: String
} deriving (Eq, Ord)

instance Show PPA where
        show p = concat ["ppa:", ppaAccount p, "/", ppaArchive p]

instance IsString PPA where
        -- | Parse strings like "ppa:zfs-native/stable" into a PPA.
        fromString s =
                let
                        [_, ppa] = split "ppa:" s
                        [acct, arch] = split "/" ppa
                in
                        PPA acct arch

-- | Adds a PPA to the local system repositories.
addPpa :: PPA -> Property NoInfo
addPpa p =
        cmdPropertyEnv "apt-add-repository" ["--yes", show p] Apt.noninteractiveEnv
        `assume` MadeChange
        `describe` ("Added PPA " ++ (show p))
        `requires` installed

-- | A repository key ID to be downloaded with apt-key.
data AptKeyId = AptKeyId {
        akiName :: String,
        akiId :: String,
        akiServer :: String
        } deriving (Eq, Ord)

instance Show AptKeyId where
        show k = unwords ["Apt Key", akiName k, akiId k, "from", akiServer k]

-- | Adds an 'AptKeyId' from the specified GPG server.
addKeyId :: AptKeyId -> Property NoInfo
addKeyId keyId =
        check keyTrusted akcmd
        `describe` (unwords ["Add third-party Apt key", show keyId])
  where
        akcmd =
                cmdProperty "apt-key" ["adv", "--keyserver", akiServer keyId, "--recv-keys", akiId keyId]
        keyTrusted =
                let
                        pks ls = concatMap (drop 1 . split "/")
                                $ concatMap (take 1 . drop 1 . words)
                                $ filter (\l -> "pub" `isPrefixOf` l)
                                        $ lines ls
                        nkid = take 8 (akiId keyId)
                in
                        (isInfixOf [nkid] . pks) <$> readProcess "apt-key" ["list"]

-- | An Apt source line that apt-add-repository will just add to
-- sources.list. It's also an instance of both 'Show' and 'IsString' to make
-- using 'OverloadedStrings' in the configuration file easier.
--
-- | FIXME there's apparently an optional "options" fragment that I've
-- definitely not parsed here.
data AptSource = AptSource {
        -- | The URL hosting the repository
        asURL :: Apt.Url,

        -- | The operating system suite
        asSuite :: String,

        -- | The list of components to install from this repository.
        asComponents :: [String]
        } deriving (Eq, Ord)

instance Show AptSource where
        show asrc = unwords ["deb", asURL asrc, asSuite asrc, unwords . asComponents $ asrc]

instance IsString AptSource where
        fromString s =
                let
                        url:suite:comps = drop 1 . words $ s
                in
                        AptSource url suite comps

-- | A repository for apt-add-source, either a PPA or a regular repository line.
data AptRepository = AptRepositoryPPA PPA | AptRepositorySource AptSource

-- | Adds an 'AptRepository' using apt-add-source.
addRepository :: AptRepository -> Property NoInfo
addRepository (AptRepositoryPPA p) = addPpa p
addRepository (AptRepositorySource src) =
        check repoExists addSrc
        `describe` unwords ["Adding APT repository", show src]
        `requires` installed
  where
        allSourceLines =
                readProcess "/bin/sh" ["-c", "cat /etc/apt/sources.list /etc/apt/sources.list.d/*"]
        activeSources = map (\s -> fromString s :: AptSource )
                . filter (not . isPrefixOf "#")
                . filter (/= "") . lines <$> allSourceLines
        repoExists = isInfixOf [src] <$> activeSources
        addSrc = cmdProperty "apt-add-source" [show src]

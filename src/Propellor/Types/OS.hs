{-# LANGUAGE DeriveDataTypeable #-}

module Propellor.Types.OS (
	System(..),
	Distribution(..),
	DebianSuite(..),
	FreeBSDRelease(..),
	FBSDVersion(..),
	isStable,
	Release,
	Architecture,
	HostName,
	UserName,
	User(..),
	Group(..),
	userGroup,
	Port(..),
	fromPort,
) where

import Network.BSD (HostName)
import Data.Typeable
import Data.String

-- | High level description of a operating system.
data System = System Distribution Architecture
	deriving (Show, Eq, Typeable)

data Distribution
	= Debian DebianSuite
	| Buntish Release -- ^ A well-known Debian derivative founded by a space tourist. The actual name of this distribution is not used in Propellor per <http://joeyh.name/blog/entry/trademark_nonsense/>)
	| FreeBSD FreeBSDRelease
	deriving (Show, Eq)

-- | Debian has several rolling suites, and a number of stable releases,
-- such as Stable "jessie".
data DebianSuite = Experimental | Unstable | Testing | Stable Release
	deriving (Show, Eq)

-- | FreeBSD breaks their releases into "Production" and "Legacy".
data FreeBSDRelease = FBSDProduction FBSDVersion | FBSDLegacy FBSDVersion
  deriving (Show, Eq)

data FBSDVersion = FBSD101 | FBSD102 | FBSD093
  deriving (Eq)

instance IsString FBSDVersion where
	fromString "10.1-RELEASE" = FBSD101
	fromString "10.2-RELEASE" = FBSD102
	fromString "9.3-RELEASE" = FBSD093
	fromString _ = error "Invalid FreeBSD release"

instance Show FBSDVersion where
	show FBSD101 = "10.1-RELEASE"
	show FBSD102 = "10.2-RELEASE"
	show FBSD093 = "9.3-RELEASE"

isStable :: DebianSuite -> Bool
isStable (Stable _) = True
isStable _ = False

type Release = String
type Architecture = String

type UserName = String

newtype User = User UserName
	deriving (Eq, Ord, Show)

newtype Group = Group String
	deriving (Eq, Ord, Show)

-- | Makes a Group with the same name as the User.
userGroup :: User -> Group
userGroup (User u) = Group u

newtype Port = Port Int
	deriving (Eq, Show)

fromPort :: Port -> String
fromPort (Port p) = show p

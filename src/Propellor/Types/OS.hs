{-# LANGUAGE DeriveDataTypeable #-}

module Propellor.Types.OS
  ( System (..),
    Distribution (..),
    TargetOS (..),
    DebianKernel (..),
    DebianSuite (..),
    FreeBSDRelease (..),
    FBSDVersion (..),
    CentOSVariant (..),
    CentOSVersion (..),
    isStable,
    Release,
    Architecture (..),
    architectureToDebianArchString,
    HostName,
    UserName,
    User (..),
    Group (..),
    userGroup,
    Port (..),
    systemToTargetOS,
  )
where

import Data.String
import Data.Typeable
import Network.Socket (HostName)
import Propellor.Types.ConfigurableValue

-- | High level description of a operating system.
data System = System Distribution Architecture
  deriving (Show, Eq, Typeable)

data Distribution
  = Debian DebianKernel DebianSuite
  | -- | A well-known Debian derivative founded by a space tourist. The actual name of this distribution is not used in Propellor per <http://joeyh.name/blog/entry/trademark_nonsense/>
    Buntish Release
  | ArchLinux
  | FreeBSD FreeBSDRelease
  | CentOS CentOSVariant
  deriving (Show, Eq)

-- | Properties can target one or more OS's; the targets are part
-- of the type of the property, so need to be kept fairly simple.
data TargetOS
  = OSDebian
  | OSBuntish
  | OSArchLinux
  | OSFreeBSD
  | OSCentOS
  deriving (Show, Eq, Ord)

systemToTargetOS :: System -> TargetOS
systemToTargetOS (System (Debian _ _) _) = OSDebian
systemToTargetOS (System (Buntish _) _) = OSBuntish
systemToTargetOS (System (ArchLinux) _) = OSArchLinux
systemToTargetOS (System (FreeBSD _) _) = OSFreeBSD
systemToTargetOS (System (CentOS _) _) = OSCentOS

-- | Most of Debian ports are based on Linux. There also exist hurd-i386,
-- kfreebsd-i386, kfreebsd-amd64 ports
data DebianKernel = Linux | KFreeBSD | Hurd
  deriving (Show, Eq)

-- | Debian has several rolling suites, and a number of stable releases,
-- such as Stable "buster".
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

instance ConfigurableValue FBSDVersion where
  val FBSD101 = "10.1-RELEASE"
  val FBSD102 = "10.2-RELEASE"
  val FBSD093 = "9.3-RELEASE"

instance Show FBSDVersion where
  show = val

-- | CentOS breaks their releases into two variants "Linux" and "Stream".
data CentOSVariant = CentOSLinux CentOSVersion | CentOSStream CentOSVersion
  deriving (Show, Eq)

data CentOSVersion = CentOS7 | CentOS8 | CentOS9
  deriving (Eq)

instance IsString CentOSVersion where
  fromString "7-2009" = CentOS7
  fromString "8" = CentOS8
  fromString "9" = CentOS9
  fromString _ = error "Invalid CentOS Version"

instance ConfigurableValue CentOSVersion where
  val CentOS7 = "7-2009"
  val CentOS8 = "8"
  val CentOS9 = "9"

instance Show CentOSVersion where
  show = val

isStable :: DebianSuite -> Bool
isStable (Stable _) = True
isStable _ = False

type Release = String

-- | Many of these architecture names are based on the names used by
-- Debian, with a few exceptions for clarity.
data Architecture
  = -- | 64 bit Intel, called "amd64" in Debian
    X86_64
  | -- | 32 bit Intel, called "i386" in Debian
    X86_32
  | ARMHF
  | ARMEL
  | PPC
  | PPC64
  | SPARC
  | SPARC64
  | MIPS
  | MIPSEL
  | MIPS64EL
  | SH4
  | -- | Itanium
    IA64
  | S390
  | S390X
  | ALPHA
  | HPPA
  | M68K
  | ARM64
  | -- | New Linux ABI for 64 bit CPUs using 32-bit integers. Not widely used.
    X32
  deriving (Show, Eq)

architectureToDebianArchString :: Architecture -> String
architectureToDebianArchString X86_64 = "amd64"
architectureToDebianArchString X86_32 = "i386"
architectureToDebianArchString ARMHF = "armhf"
architectureToDebianArchString ARMEL = "armel"
architectureToDebianArchString PPC = "powerpc"
architectureToDebianArchString PPC64 = "ppc64el"
architectureToDebianArchString SPARC = "sparc"
architectureToDebianArchString SPARC64 = "sparc64"
architectureToDebianArchString MIPS = "mips"
architectureToDebianArchString MIPSEL = "mipsel"
architectureToDebianArchString MIPS64EL = "mips64el"
architectureToDebianArchString SH4 = "sh"
architectureToDebianArchString IA64 = "ia64"
architectureToDebianArchString S390 = "s390"
architectureToDebianArchString S390X = "s390x"
architectureToDebianArchString ALPHA = "alpha"
architectureToDebianArchString HPPA = "hppa"
architectureToDebianArchString M68K = "m68k"
architectureToDebianArchString ARM64 = "arm64"
architectureToDebianArchString X32 = "x32"

type UserName = String

newtype User = User UserName
  deriving (Eq, Ord, Show)

instance ConfigurableValue User where
  val (User n) = n

newtype Group = Group String
  deriving (Eq, Ord, Show)

instance ConfigurableValue Group where
  val (Group n) = n

-- | Makes a Group with the same name as the User.
userGroup :: User -> Group
userGroup (User u) = Group u

newtype Port = Port Int
  deriving (Eq, Ord, Show)

instance ConfigurableValue Port where
  val (Port p) = show p

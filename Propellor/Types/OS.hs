module Propellor.Types.OS where

type UserName = String
type GroupName = String

-- | High level descritption of a operating system.
data System = System Distribution Architecture
	deriving (Show, Eq)

data Distribution
	= Debian DebianSuite
	| Ubuntu Release
	deriving (Show, Eq)

data DebianSuite = Experimental | Unstable | Testing | Stable | DebianRelease Release
	deriving (Show, Eq)

type Release = String
type Architecture = String

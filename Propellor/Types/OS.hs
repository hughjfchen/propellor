module Propellor.Types.OS where

type HostName = String
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

-- | The release that currently corresponds to stable.
stableRelease :: DebianSuite
stableRelease = DebianRelease "wheezy"

isStable :: DebianSuite -> Bool
isStable s = s == Stable || s == stableRelease

type Release = String
type Architecture = String

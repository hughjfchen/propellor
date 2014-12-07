module Propellor.Types.OS (
	HostName,
	UserName,
	GroupName,
	System(..),
	Distribution(..),
	DebianSuite(..),
	isStable,
	Release,
	Architecture,
) where

import Network.BSD (HostName)

type UserName = String
type GroupName = String

-- | High level description of a operating system.
data System = System Distribution Architecture
	deriving (Show, Eq)

data Distribution
	= Debian DebianSuite
	| Ubuntu Release
	deriving (Show, Eq)

-- | Debian has several rolling suites, and a number of stable releases,
-- such as Stable "wheezy".
data DebianSuite = Experimental | Unstable | Testing | Stable Release
	deriving (Show, Eq)

isStable :: DebianSuite -> Bool
isStable (Stable _) = True
isStable _ = False

type Release = String
type Architecture = String

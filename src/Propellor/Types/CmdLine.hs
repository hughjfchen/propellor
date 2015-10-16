{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving #-}

module Propellor.Types.CmdLine where

import Propellor.Types.OS
import Propellor.Types.PrivData

import System.Posix.Types
import Data.Typeable
import Data.Monoid

-- | All the command line actions that propellor can perform.
data CmdLine
	= Run HostName
	| Spin [HostName] (Maybe HostName)
	| SimpleRun HostName
	| ControlledRun HostName ControllerChain
	| Set PrivDataField Context
	| Unset PrivDataField Context
	| Dump PrivDataField Context
	| Edit PrivDataField Context
	| ListFields
	| AddKey String
	| RmKey String
	| Merge
	| Serialized CmdLine
	| Continue CmdLine
	| Update (Maybe HostName)
	| Relay HostName
	| DockerInit HostName
	| DockerChain HostName String
	| ChrootChain HostName FilePath Bool Bool
	| GitPush Fd Fd
	| Check
	deriving (Read, Show, Eq)

-- | List of hosts that acted as controllers to cause a host to be spinned.
newtype ControllerChain = ControllerChain [HostName]
	deriving (Read, Show, Eq, Typeable, Monoid)

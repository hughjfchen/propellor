{-# LANGUAGE FlexibleInstances, DeriveDataTypeable #-}

module Propellor.Types.Bootloader where

import Propellor.Types
import Propellor.Types.Info

-- | Boot loader installed on a host.
data BootloaderInstalled
	= GrubInstalled
	| FlashKernelInstalled
	| UbootInstalled (FilePath -> FilePath -> Property Linux)
	deriving (Typeable)

instance Show BootloaderInstalled where
	show GrubInstalled = "GrubInstalled"
	show FlashKernelInstalled = "FlashKernelInstalled"
	show (UbootInstalled _) = "UbootInstalled"

instance IsInfo [BootloaderInstalled] where
	propagateInfo _ = PropagateInfo False

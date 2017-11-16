{-# LANGUAGE FlexibleInstances, DeriveDataTypeable #-}

module Propellor.Types.Bootloader where

import Propellor.Types.Info

-- | Boot loader installed on a host.
data BootloaderInstalled
	= GrubInstalled
	| FlashKernelInstalled
	deriving (Typeable, Show)

instance IsInfo [BootloaderInstalled] where
	propagateInfo _ = PropagateInfo False

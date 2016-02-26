-- | Functions running zfs processes.
--
-- Copyright 2016 Evan Cofsky <evan@theunixman.com>
-- License: BSD 2-clause

module Propellor.Property.ZFS.Process where

import Propellor.Base
import Data.String.Utils (split)
import Data.List

-- | Gets the properties of a ZFS volume.
zfsGetProperties ::  ZFS -> IO ZFSProperties
zfsGetProperties z =
  let
    plist = fromPropertyList . map (\(_:k:v:_) -> (k, v)) . (map (split "\t"))
  in
    do
      plist <$> runZfs "get" [Just "-H", Just "-p", Just "all"] z

zfsExists :: ZFS -> IO Bool
zfsExists z =
	any id . map (isInfixOf (zfsName z))  <$> runZfs "list" [Just "-H"] z

-- | Runs the zfs command with the arguments.
--
-- Runs the command with -H which will skip the header line and
-- separate all fields with tabs.
--
-- Replaces Nothing in the argument list with the ZFS pool/dataset.
runZfs :: String -> [Maybe String] -> ZFS -> IO [String]
runZfs cmd args z =
	let
		(p, a) = zfsCommand cmd args z
	in
		lines <$> readProcess p a

-- | Return the ZFS command line suitable for readProcess or cmdProperty.
zfsCommand :: String -> [Maybe String] -> ZFS -> (String, [String])
zfsCommand cmd args z = ("zfs", cmd:(map (maybe (zfsName z) id) args))

{-# LANGUAGE FlexibleContexts #-}

module Propellor.Property.Partition where

import Propellor
import qualified Propellor.Property.Apt as Apt

-- | Filesystems etc that can be used for a partition.
data Fs = EXT2 | EXT3 | EXT4 | BTRFS | REISERFS | XFS | FAT | VFAT | NTFS | LinuxSwap
	deriving (Show)

data Eep = YesReallyFormatPartition

-- | Formats a partition.
formatted :: Eep -> Fs -> FilePath -> Property NoInfo
formatted = formatted' []

-- | Options passed to a mkfs.* command when making a filesystem.
--
-- Eg, ["-m0"]
type MkfsOpts = [String]

formatted' :: MkfsOpts -> Eep -> Fs -> FilePath -> Property NoInfo
formatted' opts YesReallyFormatPartition fs dev = 
	cmdProperty cmd opts' `requires` Apt.installed [pkg]
  where
	(cmd, opts', pkg) = case fs of
		EXT2 -> ("mkfs.ext2", q $ eff optsdev, "e2fsprogs")
		EXT3 -> ("mkfs.ext3", q $ eff optsdev, "e2fsprogs")
		EXT4 -> ("mkfs.ext4", q $ eff optsdev, "e2fsprogs")
		BTRFS -> ("mkfs.btrfs", optsdev, "btrfs-tools")
		REISERFS -> ("mkfs.reiserfs", q $ "-ff":optsdev, "reiserfsprogs")
		XFS -> ("mkfs.xfs", "-f":q optsdev, "xfsprogs")
		FAT -> ("mkfs.fat", optsdev, "dosfstools")
		VFAT -> ("mkfs.vfat", optsdev, "dosfstools")
		NTFS -> ("mkfs.ntfs", q $ eff optsdev, "ntfs-3g")
		LinuxSwap -> ("mkswap", optsdev, "util-linux")
	optsdev = opts++[dev]
	-- -F forces creating a filesystem even if the device already has one
	eff l = "-F":l
	-- Be quiet.
	q l = "-q":l

-- | Uses the kpartx utility to create device maps for partitions contained
-- within a disk image file. The resulting devices are passed to the
-- property, which can operate on them. Always cleans up after itself,
-- by removing the device maps after the property is run.
kpartx :: FilePath -> ([FilePath] -> Property NoInfo) -> Property NoInfo
kpartx diskimage mkprop = go `requires` Apt.installed ["kpartx"]
  where
	go = property (propertyDesc (mkprop [])) $ do
		cleanup -- idempotency
		s <- liftIO $ readProcess "kpartx" ["-avs", diskimage]
		r <- ensureProperty (mkprop (devlist s))
		cleanup
		return r
	devlist = mapMaybe (finddev . words) . lines
	finddev ("add":"map":s:_) = Just ("/dev/mapper/" ++ s)
	finddev _ = Nothing
	cleanup = void $ liftIO $ boolSystem "kpartx" [Param "-d", File diskimage]

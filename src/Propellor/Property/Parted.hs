{-# LANGUAGE FlexibleContexts #-}

module Propellor.Property.Parted (
	-- * Types
	TableType(..),
	PartTable(..),
	partTableSize,
	Partition(..),
	mkPartition,
	Partition.Fs(..),
	PartSize(..),
	ByteSize,
	toPartSize,
	fromPartSize,
	reducePartSize,
	Partition.MkfsOpts,
	PartType(..),
	PartFlag(..),
	-- * Properties
	partitioned,
	parted,
	Eep(..),
	installed,
	-- * Partition table sizing
	calcPartTable,
	DiskSize(..),
	DiskPart,
	DiskSpaceUse(..),
	useDiskSpace,
	defSz,
	fudgeSz,
) where

import Propellor.Base
import Propellor.Property.Parted.Types
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.Pacman as Pacman
import qualified Propellor.Property.Partition as Partition
import Propellor.Types.PartSpec (PartSpec)
import Utility.DataUnits

import System.Posix.Files
import Data.List (genericLength)

data Eep = YesReallyDeleteDiskContents

-- | Partitions a disk using parted, and formats the partitions.
--
-- The FilePath can be a block device (eg, \/dev\/sda), or a disk image file.
--
-- This deletes any existing partitions in the disk! Use with EXTREME caution!
partitioned :: Eep -> FilePath -> PartTable -> Property DebianLike
partitioned eep disk (PartTable tabletype parts) = property' desc $ \w -> do
	isdev <- liftIO $ isBlockDevice <$> getFileStatus disk
	ensureProperty w $ combineProperties desc $ props
		& parted eep disk partedparams
		& if isdev
			then formatl (map (\n -> disk ++ show n) [1 :: Int ..])
			else Partition.kpartx disk (formatl . map Partition.partitionLoopDev)
  where
	desc = disk ++ " partitioned"
	formatl devs = combineProperties desc (toProps $ map format (zip parts devs))
	partedparams = concat $ mklabel : mkparts (1 :: Integer) mempty parts []
	format (p, dev) = Partition.formatted' (partMkFsOpts p)
		Partition.YesReallyFormatPartition (partFs p) dev
	mklabel = ["mklabel", pval tabletype]
	mkflag partnum (f, b) =
		[ "set"
		, show partnum
		, pval f
		, pval b
		]
	mkpart partnum offset p =
		[ "mkpart"
		, pval (partType p)
		, pval (partFs p)
		, pval offset
		, pval (offset <> partSize p)
		] ++ case partName p of
			Just n -> ["name", show partnum, n]
			Nothing -> []
	mkparts partnum offset (p:ps) c = 
		mkparts (partnum+1) (offset <> partSize p) ps
			(c ++ mkpart partnum offset p : map (mkflag partnum) (partFlags p))
	mkparts _ _ [] c = c

-- | Runs parted on a disk with the specified parameters.
--
-- Parted is run in script mode, so it will never prompt for input.
-- It is asked to use cylinder alignment for the disk.
parted :: Eep -> FilePath -> [String] -> Property (DebianLike + ArchLinux)
parted YesReallyDeleteDiskContents disk ps = p `requires` installed
  where
	p = cmdProperty "parted" ("--script":"--align":"cylinder":disk:ps)
		`assume` MadeChange

-- | Gets parted installed.
installed :: Property (DebianLike + ArchLinux)
installed = Apt.installed ["parted"] `pickOS` Pacman.installed ["parted"]

-- | Gets the total size of the disk specified by the partition table.
partTableSize :: PartTable -> ByteSize
partTableSize (PartTable _ ps) = fromPartSize $
	mconcat (partitionTableOverhead : map partSize ps)

-- | Some disk is used to store the partition table itself. Assume less
-- than 1 mb.
partitionTableOverhead :: PartSize
partitionTableOverhead = MegaBytes 1

-- | Calculate a partition table, for a given size of disk.
--
-- For example:
--
-- >	calcPartTable (DiskSize (1024 * 1024 * 1024 * 100)) MSDOS
-- > 		[ partition EXT2 `mountedAt` "/boot"
-- > 			`setSize` MegaBytes 256
-- > 			`setFlag` BootFlag
-- >		, partition EXT4 `mountedAt` "/"
-- >			`useDisk` RemainingSpace
-- >		]
calcPartTable :: DiskSize -> TableType -> [PartSpec DiskPart] -> PartTable
calcPartTable (DiskSize disksize) tt l = PartTable tt (map go l)
  where
	go (_, _, mkpart, FixedDiskPart) = mkpart defSz
	go (_, _, mkpart, DynamicDiskPart (Percent p)) = mkpart $ toPartSize $
		diskremainingafterfixed * fromIntegral p `div` 100
	go (_, _, mkpart, DynamicDiskPart RemainingSpace) = mkpart $ toPartSize $
		diskremaining `div` genericLength (filter isremainingspace l)
	diskremainingafterfixed = 
		disksize - sumsizes (filter isfixed l)
	diskremaining =
		disksize - sumsizes (filter (not . isremainingspace) l)
	sumsizes = sum . map fromPartSize . (partitionTableOverhead :) .
		map (partSize . go)
	isfixed (_, _, _, FixedDiskPart) = True
	isfixed _ = False
	isremainingspace (_, _, _, DynamicDiskPart RemainingSpace) = True
	isremainingspace _ = False

-- | Size of a disk, in bytes.
newtype DiskSize = DiskSize ByteSize
	deriving (Show)

data DiskPart = FixedDiskPart | DynamicDiskPart DiskSpaceUse

data DiskSpaceUse = Percent Int | RemainingSpace

instance Monoid DiskPart
  where
	mempty = FixedDiskPart
	mappend FixedDiskPart FixedDiskPart = FixedDiskPart
	mappend (DynamicDiskPart (Percent a)) (DynamicDiskPart (Percent b)) = DynamicDiskPart (Percent (a + b))
	mappend (DynamicDiskPart RemainingSpace) (DynamicDiskPart RemainingSpace) = DynamicDiskPart RemainingSpace
	mappend (DynamicDiskPart (Percent a)) _ = DynamicDiskPart (Percent a)
	mappend _ (DynamicDiskPart (Percent b)) = DynamicDiskPart (Percent b)
	mappend (DynamicDiskPart RemainingSpace) _ = DynamicDiskPart RemainingSpace
	mappend _ (DynamicDiskPart RemainingSpace) = DynamicDiskPart RemainingSpace

-- | Make a partition use some percentage of the size of the disk
-- (less all fixed size partitions), or the remaining space in the disk.
useDiskSpace :: PartSpec DiskPart -> DiskSpaceUse -> PartSpec DiskPart
useDiskSpace (mp, o, p, _) diskuse = (mp, o, p, DynamicDiskPart diskuse)

-- | Default partition size when not otherwize specified is 128 MegaBytes.
defSz :: PartSize
defSz = MegaBytes 128

-- | When a partition is sized to fit the files that live in it,
-- this fudge factor is added to the size of the files. This is necessary
-- since filesystems have some space overhead.
-- 
-- Add 2% for filesystem overhead. Rationalle for picking 2%:
-- A filesystem with 1% overhead might just sneak by as acceptable.
-- Double that just in case. Add an additional 3 mb to deal with
-- non-scaling overhead of filesystems (eg, superblocks). 
-- Add an additional 200 mb for temp files, journals, etc.
fudgeSz :: PartSize -> PartSize
fudgeSz (MegaBytes n) = MegaBytes (n + n `div` 100 * 2 + 3 + 200)

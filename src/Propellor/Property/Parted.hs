{-# LANGUAGE FlexibleContexts #-}

module Propellor.Property.Parted (
	TableType(..),
	PartTable(..),
	PartType(..),
	FsType,
	PartFlag(..),
	Eep(..),
	partitioned,
	parted,
	installed,
) where

import Propellor
import qualified Propellor.Property.Apt as Apt
import Utility.DataUnits
import Data.Char

class PartedVal a where
	val :: a -> String

-- | Types of partition tables supported by parted.
data TableType = MSDOS | GPT | AIX | AMIGA | BSD | DVH | LOOP | MAC | PC98 | SUN
	deriving (Show)

instance PartedVal TableType where
	val = map toLower . show

-- | A disk's partition table.
data PartTable = PartTable TableType [Partition]
	deriving (Show)

instance Monoid PartTable where
	-- | default TableType is MSDOS
	mempty = PartTable MSDOS []
	-- | uses the TableType of the second parameter
	mappend (PartTable _l1 ps1) (PartTable l2 ps2) = PartTable l2 (ps1 ++ ps2)

-- | A partition on the disk.
data Partition = Partition
	{ partType :: PartType
	, partFs :: FsType
	, partSize :: ByteSize
	, partFlags :: [(PartFlag, Bool)] -- ^ flags can be set or unset (parted may set some flags by default)
	, partName :: Maybe String -- ^ optional name for partition (only works for GPT, PC98, MAC)
	}
	deriving (Show)

-- | Type of a partition.
data PartType = Primary | Logical | Extended
	deriving (Show)

instance PartedVal PartType where
	val Primary = "primary"
	val Logical = "logical"
	val Extended = "extended"

-- | Eg, "ext4" or "fat16" or "ntfs" or "hfs+" or "linux-swap"
type FsType = String

-- | Flags that can be set on a partition.
data PartFlag = BootFlag | RootFlag | SwapFlag | HiddenFlag | RaidFlag | LvmFlag | LbaFlag | LegacyBootFlag | IrstFlag | EspFlag | PaloFlag
	deriving (Show)

instance PartedVal PartFlag where
	val BootFlag = "boot"
	val RootFlag = "root"
	val SwapFlag = "swap"
	val HiddenFlag = "hidden"
	val RaidFlag = "raid"
	val LvmFlag = "lvm"
	val LbaFlag = "lba"
	val LegacyBootFlag = "legacy_boot"
	val IrstFlag = "irst"
	val EspFlag = "esp"
	val PaloFlag = "palo"

instance PartedVal Bool where
	val True = "on"
	val False = "off"

data Eep = YesReallyDeleteDiskContents

-- | Partitions a disk using parted. Does not mkfs filesystems.
--
-- The FilePath can be a disk device (eg, /dev/sda), or a disk image file.
--
-- This deletes any existing partitions in the disk! Use with EXTREME caution!
partitioned :: Eep -> FilePath -> PartTable -> Property NoInfo
partitioned eep disk (PartTable tabletype parts) = 
	parted eep disk (concat (setunits : mklabel : mkparts (1 :: Integer) 0 parts []))
		`describe` (disk ++ " partitioned")
  where
	mklabel = ["mklabel", val tabletype]
	mkflag partnum (f, b) =
		[ "set"
		, show partnum
		, val f
		, val b
		]
	setunits = ["unit", "B"]
	mkpart partnum offset p =
		[ "mkpart"
		, show partnum
		, val (partType p)
		, partFs p
		, show offset
		, show (offset + partSize p)
		] ++ case partName p of
			Just n -> ["name", show partnum, n]
			Nothing -> []
	mkparts partnum offset (p:ps) c = 
		mkparts (partnum+1) (offset + partSize p) ps
			(c ++ mkpart partnum offset p : map (mkflag partnum) (partFlags p))
	mkparts _ _ [] c = c

-- | Runs parted on a disk with the specified parameters.
--
-- Parted is run in script mode, so it will never prompt for input.
-- It is asked to use optimal alignment for the disk, for best performance.
parted :: Eep -> FilePath -> [String] -> Property NoInfo
parted YesReallyDeleteDiskContents disk ps = 
	cmdProperty "parted" ("--script":"--align":"optimal":disk:ps)
		`requires` installed

-- | Gets parted installed.
installed :: Property NoInfo
installed = Apt.installed ["parted"]

-- | Maintainer: Nicolas Schodet <nico@ni.fr.eu.org>
--
-- Support for LVM logical volumes.

module Propellor.Property.Lvm (
	lvFormatted,
	installed,
	Eep(..),
	VolumeGroup(..),
	LogicalVolume(..),
) where

import Propellor
import Propellor.Base
import Utility.DataUnits
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.Mount as Mount
import qualified Propellor.Property.Partition as Partition

data Eep = YesReallyFormatLogicalVolume

type DataSize = String

newtype VolumeGroup = VolumeGroup String
data LogicalVolume = LogicalVolume String VolumeGroup

-- | Create or resize a logical volume, and make sure it is formatted.  When
-- reverted, remove the logical volume.
--
-- Example use:
--
-- > import qualified Propellor.Property.Lvm as Lvm
-- > import qualified Propellor.Property.Partition as Partition
-- > Lvm.lvFormatted Lvm.YesReallyFormatLogicalVolume
-- >         (Lvm.LogicalVolume "test" (Lvm.VolumeGroup "vg0")) "16m"
-- >         Partition.EXT4
--
-- If size and filesystem match, nothing is done.
--
-- Volume group must have been created yet.
lvFormatted
	:: Eep
	-> LogicalVolume
	-> DataSize
	-> Partition.Fs
	-> RevertableProperty DebianLike UnixLike
lvFormatted YesReallyFormatLogicalVolume lv sz fs =
	setup <!> cleanup
  where
	setup :: Property DebianLike
	setup = property' ("formatted logical volume " ++ (vglv lv)) $ \w -> do
		es <- liftIO $ vgExtentSize vg
		case es of
			Nothing -> errorMessage
				$ "can not get extent size, does volume group "
				++ vgname ++ " exists?"
			Just extentSize -> do
				case parseSize of
					Nothing -> errorMessage
						$ "can not parse volume group size"
					Just size -> do
						state <- liftIO $ lvState lv
						let rsize = roundSize extentSize size
						ensureProperty w
							$ setupprop rsize state

	cleanup :: Property UnixLike
	cleanup = property' ("removed logical volume " ++ (vglv lv)) $ \w -> do
		exists <- liftIO $ lvExist lv
		ensureProperty w $ if exists
			then removedprop
			else doNothing

	-- Parse size.
	parseSize :: Maybe Integer
	parseSize = readSize dataUnits sz

	-- Round size to next extent size multiple.
	roundSize :: Integer -> Integer -> Integer
	roundSize extentSize s =
		(s + extentSize - 1) `div` extentSize * extentSize

	-- Dispatch to the right props.
	setupprop :: Integer -> (Maybe LvState) -> Property DebianLike
	setupprop size Nothing = createdprop size `before` formatprop
	setupprop size (Just (LvState csize cfs))
		| size == csize && fsMatch fs cfs = doNothing
		| size == csize = formatprop
		| fsMatch fs cfs = tightenTargets $ resizedprop size True
		| otherwise = resizedprop size False `before` formatprop

	createdprop :: Integer -> Property UnixLike
	createdprop size =
		cmdProperty "lvcreate"
			(bytes size $ [ "-n", lvname, "--yes", vgname ])
			`assume` MadeChange

	resizedprop :: Integer -> Bool -> Property UnixLike
	resizedprop size rfs =
		cmdProperty "lvresize"
			(resizeFs rfs $ bytes size $ [ vglv lv ])
			`assume` MadeChange
	  where
		resizeFs True l = "-r" : l
		resizeFs False l = l

	removedprop :: Property UnixLike
	removedprop = cmdProperty "lvremove" [ "-f", vglv lv ]
		`assume` MadeChange

	formatprop :: Property DebianLike
	formatprop = Partition.formatted Partition.YesReallyFormatPartition
		fs (path lv)

	fsMatch :: Partition.Fs -> Maybe Partition.Fs -> Bool
	fsMatch a (Just b) = a == b
	fsMatch _ _ = False

	bytes size l = "-L" : ((show size) ++ "b") : l

	(LogicalVolume lvname vg@(VolumeGroup vgname)) = lv

-- | Make sure needed tools are installed.
installed :: RevertableProperty DebianLike DebianLike
installed = install <!> remove
  where
	install = Apt.installed ["lvm2"]
	remove = Apt.removed ["lvm2"]

data LvState = LvState Integer (Maybe Partition.Fs)

-- Check for logical volume existance.
lvExist :: LogicalVolume -> IO Bool
lvExist lv = doesFileExist (path lv)

-- Return Nothing if logical volume does not exists (or error), else return
-- its size and maybe file system.
lvState :: LogicalVolume -> IO (Maybe LvState)
lvState lv = do
	exists <- lvExist lv
	if not exists
		then return Nothing
		else do
			s <- readLvSize
			fs <- readFs
			return $ do
				size <- s
				return $ LvState size $ parseFs
					$ takeWhile (/= '\n') <$> fs
  where
	readLvSize = catchDefaultIO Nothing
		$ readish
		<$> readProcess "lvs" [ "-o", "size", "--noheadings",
			"--nosuffix", "--units", "b", vglv lv ]
	readFs = Mount.blkidTag "TYPE" (path lv)
	parseFs (Just "ext2") = Just Partition.EXT2
	parseFs (Just "ext3") = Just Partition.EXT3
	parseFs (Just "ext4") = Just Partition.EXT4
	parseFs (Just "btrfs") = Just Partition.BTRFS
	parseFs (Just "reiserfs") = Just Partition.REISERFS
	parseFs (Just "xfs") = Just Partition.XFS
	parseFs (Just "fat") = Just Partition.FAT
	parseFs (Just "vfat") = Just Partition.VFAT
	parseFs (Just "ntfs") = Just Partition.NTFS
	parseFs (Just "swap") = Just Partition.LinuxSwap
	parseFs _ = Nothing

-- Read extent size (or Nothing on error).
vgExtentSize :: VolumeGroup -> IO (Maybe Integer)
vgExtentSize (VolumeGroup vgname) =
	catchDefaultIO Nothing
		$ readish
		<$> readProcess "vgs" [ "-o", "vg_extent_size",
			"--noheadings", "--nosuffix", "--units", "b", vgname ]

-- Give "vgname/lvname" for a LogicalVolume.
vglv :: LogicalVolume -> String
vglv lv =
	vgname </> lvname
  where
	(LogicalVolume lvname (VolumeGroup vgname)) = lv

-- Give device path.
path :: LogicalVolume -> String
path lv = "/dev" </> (vglv lv)

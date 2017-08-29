-- | Maintainer: Nicolas Schodet <nico@ni.fr.eu.org>
--
-- Support for LVM logical volumes.

module Propellor.Property.Lvm (
	lvFormatted,
	Eep(..),
	installed,
) where

import Propellor
import Propellor.Base
import Utility.DataUnits
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.Mount as Mount
import qualified Propellor.Property.Partition as Partition

data Eep = YesReallyFormatLogicalVolume

type DataSize = String

type VolumeGroup = String
type LogicalVolume = String

-- | Create or resize a logical volume, and make sure it is formatted.  When
-- reverted, remove the logical volume.
--
-- Example use:
--
-- > import qualified Propellor.Property.Lvm as Lvm
-- > import qualified Propellor.Property.Partition as Partition
-- > Lvm.lvFormatted Lvm.YesReallyFormatLogicalVolume "vg0" "test" "16m"
-- >         Partition.EXT4
--
-- If size and filesystem match, nothing is done.
--
-- Volume group must have been created yet.
lvFormatted
	:: Eep
	-> VolumeGroup
	-> LogicalVolume
	-> DataSize
	-> Partition.Fs
	-> RevertableProperty DebianLike UnixLike
lvFormatted YesReallyFormatLogicalVolume vg lv sz fs =
	setup <!> cleanup
  where
	setup :: Property DebianLike
	setup = property' ("formatted logical volume " ++ lv) $ \w -> do
		es <- liftIO $ vgExtentSize vg
		case es of
			Nothing -> errorMessage
				$ "can not get extent size, does volume group "
				++ vg ++ " exists?"
			Just extentSize -> do
				case parseSize extentSize of
					Nothing -> errorMessage
						$ "can not parse volume group size"
					Just size -> do
						state <- liftIO $ lvState vg lv
						ensureProperty w
							$ setupprop size state

	cleanup :: Property UnixLike
	cleanup = property' ("removed logical volume " ++ lv) $ \w -> do
		exists <- liftIO $ lvExist vg lv
		ensureProperty w $ if exists
			then removedprop
			else doNothing

	-- Parse size and round to next extent size multiple.
	parseSize :: Integer -> Maybe Integer
	parseSize extentSize = do
		s <- readSize dataUnits sz
		return $ (s + extentSize - 1) `div` extentSize * extentSize

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
			(bytes size $ [ "-n", lv, "--yes", vg ])
			`assume` MadeChange

	resizedprop :: Integer -> Bool -> Property UnixLike
	resizedprop size rfs =
		cmdProperty "lvresize"
			(resizeFs rfs $ bytes size $ [ vg </> lv ])
			`assume` MadeChange
	  where
		resizeFs True l = "-r" : l
		resizeFs False l = l

	removedprop :: Property UnixLike
	removedprop = cmdProperty "lvremove" [ "-f", vg </> lv ]
		`assume` MadeChange

	formatprop :: Property DebianLike
	formatprop = Partition.formatted Partition.YesReallyFormatPartition fs path

	path = "/dev" </> vg </> lv

	fsMatch :: Partition.Fs -> Maybe String -> Bool
	fsMatch Partition.EXT2 (Just "ext2") = True
	fsMatch Partition.EXT3 (Just "ext3") = True
	fsMatch Partition.EXT4 (Just "ext4") = True
	fsMatch Partition.BTRFS (Just "btrfs") = True
	fsMatch Partition.REISERFS (Just "reiserfs") = True
	fsMatch Partition.XFS (Just "xfs") = True
	fsMatch Partition.FAT (Just "fat") = True
	fsMatch Partition.VFAT (Just "vfat") = True
	fsMatch Partition.NTFS (Just "ntfs") = True
	fsMatch Partition.LinuxSwap (Just "swap") = True
	fsMatch _ _ = False

	bytes size l = "-L" : ((show size) ++ "b") : l

-- | Make sure needed tools are installed.
installed :: RevertableProperty DebianLike DebianLike
installed = install <!> remove
  where
	install = Apt.installed ["lvm2"]
	remove = Apt.removed ["lvm2"]

data LvState = LvState Integer (Maybe String)

-- Check for logical volume existance.
lvExist :: VolumeGroup -> LogicalVolume -> IO Bool
lvExist vg lv =
	doesFileExist path
  where
	path = "/dev" </> vg </> lv

-- Return Nothing if logical volume does not exists (or error), else return
-- its size and maybe file system.
lvState :: VolumeGroup -> LogicalVolume -> IO (Maybe LvState)
lvState vg lv = do
	exists <- lvExist vg lv
	if not exists
		then return Nothing
		else do
			s <- readLvSize
			fs <- readFs
			return $ do
				size <- s
				return $ LvState size $ takeWhile (/= '\n') <$> fs
  where
	path = "/dev" </> vg </> lv
	readLvSize = catchDefaultIO Nothing
		$ readish
		<$> readProcess "lvs" [ "-o", "size", "--noheadings",
			"--nosuffix", "--units", "b", path ]
	readFs = Mount.blkidTag "TYPE" path

-- Read extent size (or Nothing on error).
vgExtentSize :: VolumeGroup -> IO (Maybe Integer)
vgExtentSize vg =
	catchDefaultIO Nothing
		$ readish
		<$> readProcess "vgs" [ "-o", "vg_extent_size",
			"--noheadings", "--nosuffix", "--units", "b", path ]
  where
	path = "/dev" </> vg

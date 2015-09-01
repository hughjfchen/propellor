{-# LANGUAGE FlexibleContexts #-}

module Propellor.Property.DiskImage (
	built,
	rebuilt,
	exists,
	MountPoint,
	MkPartTable,
	fitChrootSize,
	freeSpace,
	DiskImageFinalization,
	grubBooted,
	Grub.BIOS(..),
) where

import Propellor
import Propellor.Property.Chroot (Chroot)
import Propellor.Property.Chroot.Util (removeChroot)
import qualified Propellor.Property.Chroot as Chroot
import Propellor.Property.Parted
import qualified Propellor.Property.Grub as Grub
import qualified Propellor.Property.File as File

import qualified Data.Map.Strict as M
import qualified Data.ByteString.Lazy as L
import System.Posix.Files

-- | Creates a bootable disk image in the specified file.
--
-- First the specified Chroot is set up, and its properties are satisfied.
--
-- Then, the disk image is set up, and the chroot is copied into the
-- appropriate partition(s) of it.
--
-- Finally, the DiskImageFinalization property is
-- satisfied to make the disk image bootable.
-- 
-- > let chroot d = Chroot.debootstrapped (System (Debian Unstable) "amd64") mempty d
-- > 		& Apt.installed ["openssh-server"]
-- >		& ...
-- >    partitions = fitChrootSize MSDOS
-- >		[ (Just "/boot", mkPartiton EXT2)
-- >		, (Just "/", mkPartition EXT4)
-- >		, (Nothing, const (mkPartition LinuxSwap (MegaBytes 256)))
-- >		]
-- > in built chroot partitions (grubBooted PC)
built :: FilePath -> (FilePath -> Chroot) -> MkPartTable -> DiskImageFinalization -> RevertableProperty
built = built' False

-- | Like 'built', but the chroot is deleted and rebuilt from scratch each
-- time. This is more expensive, but useful to ensure reproducible results
-- when the properties of the chroot have been changed.
rebuilt :: FilePath -> (FilePath -> Chroot) -> MkPartTable -> DiskImageFinalization -> RevertableProperty
rebuilt = built' True

built' :: Bool -> FilePath -> (FilePath -> Chroot) -> MkPartTable -> DiskImageFinalization -> RevertableProperty
built' rebuild img mkchroot mkparttable final = 
	(mkimg <!> unmkimg) 
		`requires` Chroot.provisioned (mkchroot chrootdir)
		`requires` (handlerebuild <!> doNothing)
		`describe` desc
  where
	desc = "built disk image " ++ img
	unmkimg = File.notPresent img
	chrootdir = img ++ ".chroot"
	mkimg = property desc $ do
		szm <- liftIO $ M.map toPartSize <$> dirSizes chrootdir
		-- tie the knot!
		let (mnts, t) = mkparttable (map (getMountSz szm) mnts)
		let disksz = partTableSize t
		ensureProperty $
			exists img disksz
				`before`
			partitioned YesReallyDeleteDiskContents img t
	handlerebuild
		| rebuild = property desc $ do
			liftIO $ removeChroot chrootdir
			return MadeChange
		| otherwise = doNothing

-- | Ensures that a disk image file of the specified size exists.
-- 
-- If the file doesn't exist, or is too small, creates a new one, full of 0's.
--
-- If the file is too large, truncates it down to the specified size.
exists :: FilePath -> ByteSize -> Property NoInfo
exists img sz = property ("disk image exists" ++ img) $ liftIO $ do
	ms <- catchMaybeIO $ getFileStatus img
	case ms of
		Just s 
			| toInteger (fileSize s) == toInteger sz -> return NoChange
			| toInteger (fileSize s) > toInteger sz -> do
				setFileSize img (fromInteger sz)
				return MadeChange
		_ -> do
			L.writeFile img (L.replicate (fromIntegral sz) 0)
			return MadeChange

-- | Generates a map of the sizes of the contents of 
-- every directory in a filesystem tree.
--
-- Should be same values as du -b
dirSizes :: FilePath -> IO (M.Map FilePath Integer)
dirSizes top = go M.empty top [top]
  where
	go m _ [] = return m
	go m dir (i:is) = do
		s <- getSymbolicLinkStatus i
		let sz = fromIntegral (fileSize s)
		if isDirectory s
			then do
				subm <- go M.empty i =<< dirContents i
				let sz' = M.foldr' (+) sz 
					(M.filterWithKey (const . subdirof i) subm)
				go (M.insertWith (+) i sz' (M.union m subm)) dir is
			else go (M.insertWith (+) dir sz m) dir is
	subdirof parent i = not (i `equalFilePath` parent) && takeDirectory i `equalFilePath` parent

-- | Where a partition is mounted. Use Nothing for eg, LinuxSwap.
type MountPoint = Maybe FilePath

getMountSz :: (M.Map FilePath PartSize) -> MountPoint -> PartSize
getMountSz _ Nothing = defSz
getMountSz szm (Just mntpt) = M.findWithDefault defSz mntpt szm
	
defSz :: PartSize
defSz = MegaBytes 128

-- | This is provided with a list of the sizes of directories in the chroot
-- under each mount point. The input list corresponds to the list of mount
-- points that the function returns! This trick is accomplished by 
-- exploiting laziness and tying the knot.
--
-- (Partitions that are not to be mounted (ie, LinuxSwap), or that have
-- no corresponding directory in the chroot will have 128 MegaBytes
-- provided as a default size.)
type MkPartTable = [PartSize] -> ([MountPoint], PartTable)

-- | The constructor for each Partition is passed the size of the files
-- from the chroot that will be put in that partition.
fitChrootSize :: TableType -> [(MountPoint, PartSize -> Partition)] -> MkPartTable
fitChrootSize tt l basesizes = (mounts, parttable)
  where
	(mounts, sizers) = unzip l
	parttable = PartTable tt (map (uncurry id) (zip sizers basesizes))

-- | After populating the partitions with files from the chroot,
-- they will have remaining free space equal to the sizes of the input
-- partitions.
freeSpace :: TableType -> [(MountPoint, Partition)] -> MkPartTable
freeSpace tt = fitChrootSize tt . map (\(mnt, p) -> (mnt, adjustsz p))
  where
	adjustsz p basesize = p { partSize = partSize p <> basesize }

-- | A pair of properties. The first property is satisfied within the
-- chroot, and is typically used to download the boot loader.
-- The second property is satisfied chrooted into the resulting
-- disk image, and will typically take care of installing the boot loader
-- to the disk image.
type DiskImageFinalization = (Property NoInfo, Property NoInfo)

-- | Makes grub be the boot loader of the disk image.
grubBooted :: Grub.BIOS -> DiskImageFinalization
grubBooted bios = (Grub.installed bios, undefined)

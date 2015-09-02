{-# LANGUAGE FlexibleContexts #-}

-- | Disk image generation. 
--
-- This module is designed to be imported unqualified.

module Propellor.Property.DiskImage (
	-- * Properties
	imageBuilt,
	imageRebuilt,
	imageExists,
	-- * Partition specifiction
	module Propellor.Property.Parted,
	MountPoint,
	PartSpec,
	mountedAt,
	swapPartition,
	-- * Partition sizing
	SizePartTable,
	fitChrootSize,
	freeSpace,
	-- * Finalization
	Finalization,
	grubBooted,
	Grub.BIOS(..),
) where

import Propellor
import Propellor.Property.Chroot (Chroot)
import Propellor.Property.Chroot.Util (removeChroot)
import qualified Propellor.Property.Chroot as Chroot
import qualified Propellor.Property.Grub as Grub
import qualified Propellor.Property.File as File
import Propellor.Property.Parted
import Propellor.Property.Mount
import Utility.Path

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
-- Example use:
--
-- > import Propellor.Property.DiskImage
--
-- > let chroot d = Chroot.debootstrapped (System (Debian Unstable) "amd64") mempty d
-- > 		& Apt.installed ["linux-image-amd64"]
-- >		& ...
-- >    partitions = fitChrootSize MSDOS
-- >		[ mkPartition EXT2 `mountedAt` "/boot"
-- >		, mkPartition EXT4 `mountedAt` "/"
-- >		, swapPartition (MegaBytes 256)
-- >		]
-- > in imageBuilt "/srv/images/foo.img" chroot partitions (grubBooted PC)
imageBuilt :: FilePath -> (FilePath -> Chroot) -> SizePartTable -> Finalization -> RevertableProperty
imageBuilt = imageBuilt' False

-- | Like 'built', but the chroot is deleted and rebuilt from scratch each
-- time. This is more expensive, but useful to ensure reproducible results
-- when the properties of the chroot have been changed.
imageRebuilt :: FilePath -> (FilePath -> Chroot) -> SizePartTable -> Finalization -> RevertableProperty
imageRebuilt = imageBuilt' True

imageBuilt' :: Bool -> FilePath -> (FilePath -> Chroot) -> SizePartTable -> Finalization -> RevertableProperty
imageBuilt' rebuild img mkchroot mkparttable final = 
	(mkimg <!> unmkimg) 
		-- TODO snd final
		-- TODO copy in
		-- TODO fst final
		-- TODO chroot topevel directory perm fixup
		`requires` Chroot.provisioned (mkchroot chrootdir)
		`requires` (cleanrebuild <!> doNothing)
		`describe` desc
  where
	desc = "built disk image " ++ img
	unmkimg = File.notPresent img
	chrootdir = img ++ ".chroot"
	mkimg = property desc $ do
		-- unmount helper filesystems such as proc from the chroot
		-- before getting sizes
		liftIO $ unmountBelow chrootdir
		szm <- M.mapKeys (toSysDir chrootdir) . M.map toPartSize 
			<$> liftIO (dirSizes chrootdir)
		-- tie the knot!
		-- TODO if any size is < 1 MB, use 1 MB for sanity
		let (mnts, t) = mkparttable (map (saneSz . fromMaybe defSz . getMountSz szm mnts) mnts)
		liftIO $ print (mnts, t)
		ensureProperty $
			imageExists img (partTableSize t)
				`before`
			partitioned YesReallyDeleteDiskContents img t
	cleanrebuild
		| rebuild = property desc $ do
			liftIO $ removeChroot chrootdir
			return MadeChange
		| otherwise = doNothing

-- | Ensures that a disk image file of the specified size exists.
-- 
-- If the file doesn't exist, or is too small, creates a new one, full of 0's.
--
-- If the file is too large, truncates it down to the specified size.
imageExists :: FilePath -> ByteSize -> Property NoInfo
imageExists img sz = property ("disk image exists" ++ img) $ liftIO $ do
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
	go m dir (i:is) = flip catchIO (\_ioerr -> go m dir is) $ do
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

-- | Gets the size to allocate for a particular mount point, given the
-- map of sizes.
--
-- A list of all mount points is provided, so that when eg calculating
-- the size for /, if /boot is a mount point, its size can be subtracted.
getMountSz :: (M.Map FilePath PartSize) -> [MountPoint] -> MountPoint -> Maybe PartSize
getMountSz _ _ Nothing = Nothing
getMountSz szm l (Just mntpt) = 
	fmap (`reducePartSize` childsz) (M.lookup mntpt szm)
  where
	childsz = mconcat $ catMaybes $
		map (getMountSz szm l) (filter childmntpt l)
	childmntpt Nothing = False
	childmntpt (Just d) 
		| d `equalFilePath` mntpt = False
		| otherwise = mntpt `dirContains` d

-- | From a location in a chroot (eg, /tmp/chroot/usr) to
-- the corresponding location inside (eg, /usr).
toSysDir :: FilePath -> FilePath -> FilePath
toSysDir chrootdir d = case makeRelative chrootdir d of
		"." -> "/"
		sysdir -> "/" ++ sysdir

-- | Where a partition is mounted. Use Nothing for eg, LinuxSwap.
type MountPoint = Maybe FilePath

-- | Specifies a mount point and a constructor for a Partition
-- that will later be privided with a size.
type PartSpec = (MountPoint, PartSize -> Partition)

-- | Specifies a mounted partition using a given filesystem.
mountedAt :: (PartSize -> Partition) -> FilePath -> PartSpec
mountedAt mkp mntpoint = (Just mntpoint, mkp)

-- | Specifies a swap partition of a given size.
swapPartition :: PartSize -> PartSpec
swapPartition sz = (Nothing, const (mkPartition LinuxSwap sz))

-- | Avoid partitions smaller than 1 mb; parted gets confused.
saneSz :: PartSize -> PartSize
saneSz (MegaBytes n) | n < 1 = MegaBytes 1
saneSz sz = sz

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
type SizePartTable = [PartSize] -> ([MountPoint], PartTable)

-- | The constructor for each Partition is passed the size of the files
-- from the chroot that will be put in that partition.
fitChrootSize :: TableType -> [PartSpec] -> SizePartTable
fitChrootSize tt l basesizes = (mounts, parttable)
  where
	(mounts, sizers) = unzip l
	parttable = PartTable tt (map (uncurry id) (zip sizers basesizes))

-- | After populating the partitions with files from the chroot,
-- they will have remaining free space equal to the sizes of the input
-- partitions.
freeSpace :: TableType -> [(MountPoint, Partition)] -> SizePartTable
freeSpace tt = fitChrootSize tt . map (\(mnt, p) -> (mnt, adjustsz p))
  where
	adjustsz p basesize = p { partSize = partSize p <> basesize }

-- | A pair of properties. The first property is satisfied within the
-- chroot, and is typically used to download the boot loader.
-- The second property is satisfied chrooted into the resulting
-- disk image, and will typically take care of installing the boot loader
-- to the disk image.
type Finalization = (Property NoInfo, Property NoInfo)

-- | Makes grub be the boot loader of the disk image.
grubBooted :: Grub.BIOS -> Finalization
grubBooted bios = (Grub.installed bios, undefined)

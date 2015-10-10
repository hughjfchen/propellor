-- | Disk image generation. 
--
-- This module is designed to be imported unqualified.
--
-- TODO run final
-- 
-- TODO avoid starting services while populating chroot and running final

module Propellor.Property.DiskImage (
	-- * Properties
	DiskImage,
	imageBuilt,
	imageRebuilt,
	imageBuiltFrom,
	imageExists,
	-- * Partitioning
	Partition,
	PartSize(..),
	Fs(..),
	PartSpec,
	MountPoint,
	swapPartition,
	partition,
	mountedAt,
	addFreeSpace,
	setSize,
	PartFlag(..),
	setFlag,
	TableType(..),
	extended,
	adjustp,
	-- * Finalization
	Finalization,
	grubBooted,
	Grub.BIOS(..),
	noFinalization,
) where

import Propellor.Base
import Propellor.Property.Chroot (Chroot)
import Propellor.Property.Chroot.Util (removeChroot)
import qualified Propellor.Property.Chroot as Chroot
import qualified Propellor.Property.Grub as Grub
import qualified Propellor.Property.File as File
import qualified Propellor.Property.Apt as Apt
import Propellor.Property.Parted
import Propellor.Property.Mount
import Propellor.Property.Partition
import Propellor.Property.Rsync
import Utility.Path

import Data.List (isPrefixOf)
import qualified Data.Map.Strict as M
import qualified Data.ByteString.Lazy as L
import System.Posix.Files

type DiskImage = FilePath

-- | Creates a bootable disk image.
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
-- > in imageBuilt "/srv/images/foo.img" chroot MSDOS 
-- >		[ partition EXT2 `mountedAt` "/boot"
-- >			`setFlag` BootFlag
-- >		, partition EXT4 `mountedAt` "/"
-- >			`addFreeSpace` MegaBytes 100
-- >		, swapPartition (MegaBytes 256)
-- >		] (grubBooted PC)
imageBuilt :: DiskImage -> (FilePath -> Chroot) -> TableType -> [PartSpec] -> Finalization -> RevertableProperty
imageBuilt = imageBuilt' False

-- | Like 'built', but the chroot is deleted and rebuilt from scratch each
-- time. This is more expensive, but useful to ensure reproducible results
-- when the properties of the chroot have been changed.
imageRebuilt :: DiskImage -> (FilePath -> Chroot) -> TableType -> [PartSpec] -> Finalization -> RevertableProperty
imageRebuilt = imageBuilt' True

imageBuilt' :: Bool -> DiskImage -> (FilePath -> Chroot) -> TableType -> [PartSpec] -> Finalization -> RevertableProperty
imageBuilt' rebuild img mkchroot tabletype partspec final = 
	imageBuiltFrom img chrootdir tabletype partspec (snd final)
		`requires` Chroot.provisioned chroot
		`requires` (cleanrebuild <!> doNothing)
		`describe` desc
  where
	desc = "built disk image " ++ img
	cleanrebuild
		| rebuild = property desc $ do
			liftIO $ removeChroot chrootdir
			return MadeChange
		| otherwise = doNothing
	chrootdir = img ++ ".chroot"
	chroot = mkchroot chrootdir
		-- First stage finalization.
		& fst final
		-- Avoid wasting disk image space on the apt cache
		& Apt.cacheCleaned

-- | Builds a disk image from the contents of a chroot.
--
-- The passed property is run inside the mounted disk image.
imageBuiltFrom :: DiskImage -> FilePath -> TableType -> [PartSpec] -> Property NoInfo -> RevertableProperty
imageBuiltFrom img chrootdir tabletype partspec final = mkimg <!> rmimg
  where
	desc = img ++ " built from " ++ chrootdir
	mkimg = property desc $ do
		-- unmount helper filesystems such as proc from the chroot
		-- before getting sizes
		liftIO $ unmountBelow chrootdir
		szm <- M.mapKeys (toSysDir chrootdir) . M.map toPartSize 
			<$> liftIO (dirSizes chrootdir)
		let calcsz mnts = maybe defSz fudge . getMountSz szm mnts
		-- tie the knot!
		let (mnts, t) = fitChrootSize tabletype partspec (map (calcsz mnts) mnts)
		ensureProperty $
			imageExists img (partTableSize t)
				`before`
			partitioned YesReallyDeleteDiskContents img t
				`before`
			kpartx img (partitionsPopulated chrootdir mnts)
	rmimg = File.notPresent img

partitionsPopulated :: FilePath -> [MountPoint] -> [FilePath] -> Property NoInfo
partitionsPopulated chrootdir mnts devs = property desc $ mconcat $ zipWith go mnts devs
  where
	desc = "partitions populated from " ++ chrootdir

	go Nothing _ = noChange
	go (Just mnt) dev = withTmpDir "mnt" $ \tmpdir -> bracket
		(liftIO $ mount "auto" dev tmpdir)
		(const $ liftIO $ umountLazy tmpdir)
		$ \mounted -> if mounted
			then ensureProperty $
				syncDirFiltered (filtersfor mnt) (chrootdir ++ mnt) tmpdir
			else return FailedChange

	filtersfor mnt = 
		let childmnts = map (drop (length (dropTrailingPathSeparator mnt))) $
			filter (\m -> m /= mnt && addTrailingPathSeparator mnt `isPrefixOf` m)
				(catMaybes mnts)
		in concatMap (\m -> 
			-- Include the child mount point, but exclude its contents.
			[ Include (Pattern m)
			, Exclude (filesUnder m)
			]) childmnts

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
-- (Hard links are counted multiple times for simplicity)
--
-- Should be same values as du -bl
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

getMountSz :: (M.Map FilePath PartSize) -> [MountPoint] -> MountPoint -> Maybe PartSize
getMountSz _ _ Nothing = Nothing
getMountSz szm l (Just mntpt) = 
	fmap (`reducePartSize` childsz) (M.lookup mntpt szm)
  where
	childsz = mconcat $ mapMaybe (getMountSz szm l) (filter (isChild mntpt) l)

isChild :: FilePath -> MountPoint -> Bool
isChild mntpt (Just d)
	| d `equalFilePath` mntpt = False
	| otherwise = mntpt `dirContains` d
isChild _ Nothing = False

-- | From a location in a chroot (eg, /tmp/chroot/usr) to
-- the corresponding location inside (eg, /usr).
toSysDir :: FilePath -> FilePath -> FilePath
toSysDir chrootdir d = case makeRelative chrootdir d of
		"." -> "/"
		sysdir -> "/" ++ sysdir

-- | Where a partition is mounted. Use Nothing for eg, LinuxSwap.
type MountPoint = Maybe FilePath

defSz :: PartSize
defSz = MegaBytes 128

-- Add 2% for filesystem overhead. Rationalle for picking 2%:
-- A filesystem with 1% overhead might just sneak by as acceptable.
-- Double that just in case. Add an additional 3 mb to deal with
-- non-scaling overhead, of filesystems (eg, superblocks).
fudge :: PartSize -> PartSize
fudge (MegaBytes n) = MegaBytes (n + n `div` 100 * 2 + 3)

-- | Specifies a mount point and a constructor for a Partition.
-- 
-- The size that is eventually provided is the amount of space needed to 
-- hold the files that appear in the directory where the partition is to be
-- mounted. Plus a fudge factor, since filesystems have some space
-- overhead.
--
-- (Partitions that are not to be mounted (ie, LinuxSwap), or that have
-- no corresponding directory in the chroot will have 128 MegaBytes
-- provided as a default size.)
type PartSpec = (MountPoint, PartSize -> Partition)

-- | Specifies a swap partition of a given size.
swapPartition :: PartSize -> PartSpec
swapPartition sz = (Nothing, const (mkPartition LinuxSwap sz))

-- | Specifies a partition with a given filesystem.
--
-- The partition is not mounted anywhere by default; use the combinators
-- below to configure it.
partition :: Fs -> PartSpec
partition fs = (Nothing, mkPartition fs)

-- | Specifies where to mount a partition.
mountedAt :: PartSpec -> FilePath -> PartSpec
mountedAt (_, p) mp = (Just mp, p)

-- | Adds additional free space to the partition.
addFreeSpace :: PartSpec -> PartSize -> PartSpec
addFreeSpace (mp, p) freesz = (mp, \sz -> p (sz <> freesz))

-- | Forced a partition to be a specific size, instead of scaling to the
-- size needed for the files in the chroot.
setSize :: PartSpec -> PartSize -> PartSpec
setSize (mp, p) sz = (mp, const (p sz))

-- | Sets a flag on the partition.
setFlag :: PartSpec -> PartFlag -> PartSpec
setFlag s f = adjustp s $ \p -> p { partFlags = (f, True):partFlags p }

-- | Makes a MSDOS partition be Extended, rather than Primary.
extended :: PartSpec -> PartSpec
extended s = adjustp s $ \p -> p { partType = Extended }

adjustp :: PartSpec -> (Partition -> Partition) -> PartSpec
adjustp (mp, p) f = (mp, f . p)

-- | The constructor for each Partition is passed the size of the files
-- from the chroot that will be put in that partition.
fitChrootSize :: TableType -> [PartSpec] -> [PartSize] -> ([MountPoint], PartTable)
fitChrootSize tt l basesizes = (mounts, parttable)
  where
	(mounts, sizers) = unzip l
	parttable = PartTable tt (zipWith id sizers basesizes)

-- | A pair of properties. The first property is satisfied within the
-- chroot, and is typically used to download the boot loader.
-- The second property is satisfied chrooted into the resulting
-- disk image, and will typically take care of installing the boot loader
-- to the disk image.
type Finalization = (Property NoInfo, Property NoInfo)

-- | Makes grub be the boot loader of the disk image.
-- TODO not implemented
grubBooted :: Grub.BIOS -> Finalization
grubBooted bios = (Grub.installed bios, undefined)

noFinalization :: Finalization
noFinalization = (doNothing, doNothing)

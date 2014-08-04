module Propellor.Property.File where

import Propellor
import Utility.FileMode

import System.Posix.Files
import System.PosixCompat.Types

type Line = String

-- | Replaces all the content of a file.
hasContent :: FilePath -> [Line] -> Property
f `hasContent` newcontent = fileProperty ("replace " ++ f)
	(\_oldcontent -> newcontent) f

-- | Ensures a file has contents that comes from PrivData.
--
-- The file's permissions are preserved if the file already existed.
-- Otherwise, they're set to 600.
hasPrivContent :: FilePath -> Context -> Property
hasPrivContent = hasPrivContent' writeFileProtected

-- | Leaves the file at its default or current mode,
-- allowing "private" data to be read.
--
-- Use with caution!
hasPrivContentExposed :: FilePath -> Context -> Property
hasPrivContentExposed = hasPrivContent' writeFile

hasPrivContent' :: (String -> FilePath -> IO ()) -> FilePath -> Context -> Property
hasPrivContent' writer f context = 
	withPrivData (PrivFile f) context $ \getcontent -> 
		property desc $ getcontent $ \privcontent -> 
			ensureProperty $ fileProperty' writer desc
				(\_oldcontent -> lines privcontent) f
  where
	desc = "privcontent " ++ f

-- | Ensures that a line is present in a file, adding it to the end if not.
containsLine :: FilePath -> Line -> Property
f `containsLine` l = f `containsLines` [l]

containsLines :: FilePath -> [Line] -> Property
f `containsLines` ls = fileProperty (f ++ " contains:" ++ show ls) go f
  where
	go content = content ++ filter (`notElem` content) ls

-- | Ensures that a line is not present in a file.
-- Note that the file is ensured to exist, so if it doesn't, an empty
-- file will be written.
lacksLine :: FilePath -> Line -> Property
f `lacksLine` l = fileProperty (f ++ " remove: " ++ l) (filter (/= l)) f

-- | Removes a file. Does not remove symlinks or non-plain-files.
notPresent :: FilePath -> Property
notPresent f = check (doesFileExist f) $ property (f ++ " not present") $ 
	makeChange $ nukeFile f

fileProperty :: Desc -> ([Line] -> [Line]) -> FilePath -> Property
fileProperty = fileProperty' writeFile
fileProperty' :: (FilePath -> String -> IO ()) -> Desc -> ([Line] -> [Line]) -> FilePath -> Property
fileProperty' writer desc a f = property desc $ go =<< liftIO (doesFileExist f)
  where
	go True = do
		ls <- liftIO $ lines <$> readFile f
		let ls' = a ls
		if ls' == ls
			then noChange
			else makeChange $ viaTmp updatefile f (unlines ls')
	go False = makeChange $ writer f (unlines $ a [])

	-- viaTmp makes the temp file mode 600.
	-- Replicate the original file's owner and mode.
	updatefile f' content = do
		writer f' content
		s <- getFileStatus f
		setFileMode f' (fileMode s)
		setOwnerAndGroup f' (fileOwner s) (fileGroup s)

-- | Ensures a directory exists.
dirExists :: FilePath -> Property
dirExists d = check (not <$> doesDirectoryExist d) $ property (d ++ " exists") $
	makeChange $ createDirectoryIfMissing True d

-- | Ensures that a file/dir has the specified owner and group.
ownerGroup :: FilePath -> UserName -> GroupName -> Property
ownerGroup f owner group = property (f ++ " owner " ++ og) $ do
	r <- ensureProperty $ cmdProperty "chown" [og, f]
	if r == FailedChange
		then return r
		else noChange
  where
	og = owner ++ ":" ++ group

-- | Ensures that a file/dir has the specfied mode.
mode :: FilePath -> FileMode -> Property
mode f v = property (f ++ " mode " ++ show v) $ do
	liftIO $ modifyFileMode f (\_old -> v)
	noChange

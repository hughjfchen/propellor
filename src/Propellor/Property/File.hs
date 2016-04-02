module Propellor.Property.File where

import Propellor.Base
import Utility.FileMode

import System.Posix.Files
import System.Exit

type Line = String

-- | Replaces all the content of a file.
hasContent :: FilePath -> [Line] -> Property UnixLike
f `hasContent` newcontent = fileProperty
	("replace " ++ f)
	(\_oldcontent -> newcontent) f

-- | Replaces all the content of a file, ensuring that its modes do not
-- allow it to be read or written by anyone other than the current user
hasContentProtected :: FilePath -> [Line] -> Property UnixLike
f `hasContentProtected` newcontent = fileProperty' writeFileProtected 
	("replace " ++ f)
	(\_oldcontent -> newcontent) f

-- | Ensures a file has contents that comes from PrivData.
--
-- The file's permissions are preserved if the file already existed.
-- Otherwise, they're set to 600.
hasPrivContent :: IsContext c => FilePath -> c -> Property (HasInfo + UnixLike)
hasPrivContent f = hasPrivContentFrom (PrivDataSourceFile (PrivFile f) f) f

-- | Like hasPrivContent, but allows specifying a source
-- for PrivData, rather than using PrivDataSourceFile .
hasPrivContentFrom :: (IsContext c, IsPrivDataSource s) => s -> FilePath -> c -> Property (HasInfo + UnixLike)
hasPrivContentFrom = hasPrivContent' writeFileProtected

-- | Leaves the file at its default or current mode,
-- allowing "private" data to be read.
--
-- Use with caution!
hasPrivContentExposed :: IsContext c => FilePath -> c -> Property (HasInfo + UnixLike)
hasPrivContentExposed f = hasPrivContentExposedFrom (PrivDataSourceFile (PrivFile f) f) f

hasPrivContentExposedFrom :: (IsContext c, IsPrivDataSource s) => s -> FilePath -> c -> Property (HasInfo + UnixLike)
hasPrivContentExposedFrom = hasPrivContent' writeFile

hasPrivContent' :: (IsContext c, IsPrivDataSource s) => (FilePath -> String -> IO ()) -> s -> FilePath -> c -> Property (HasInfo + UnixLike)
hasPrivContent' writer source f context = 
	withPrivData source context $ \getcontent -> 
		property' desc $ \o -> getcontent $ \privcontent -> 
			ensureProperty o $ fileProperty' writer desc
				(\_oldcontent -> privDataLines privcontent) f
  where
	desc = "privcontent " ++ f

-- | Ensures that a line is present in a file, adding it to the end if not.
containsLine :: FilePath -> Line -> Property UnixLike
f `containsLine` l = f `containsLines` [l]

containsLines :: FilePath -> [Line] -> Property UnixLike
f `containsLines` ls = fileProperty (f ++ " contains:" ++ show ls) go f
  where
	go content = content ++ filter (`notElem` content) ls

-- | Ensures that a line is not present in a file.
-- Note that the file is ensured to exist, so if it doesn't, an empty
-- file will be written.
lacksLine :: FilePath -> Line -> Property UnixLike
f `lacksLine` l = fileProperty (f ++ " remove: " ++ l) (filter (/= l)) f

lacksLines :: FilePath -> [Line] -> Property UnixLike
f `lacksLines` ls = fileProperty (f ++ " remove: " ++ show [ls]) (filter (`notElem` ls)) f

-- | Replaces the content of a file with the transformed content of another file
basedOn :: FilePath -> (FilePath, [Line] -> [Line]) -> Property UnixLike
f `basedOn` (f', a) = property' desc $ \o -> do
	tmpl <- liftIO $ readFile f'
	ensureProperty o $ fileProperty desc (\_ -> a $ lines $ tmpl) f
  where
	desc = "replace " ++ f

-- | Removes a file. Does not remove symlinks or non-plain-files.
notPresent :: FilePath -> Property UnixLike
notPresent f = check (doesFileExist f) $ property (f ++ " not present") $ 
	makeChange $ nukeFile f

fileProperty :: Desc -> ([Line] -> [Line]) -> FilePath -> Property UnixLike
fileProperty = fileProperty' writeFile
fileProperty' :: (FilePath -> String -> IO ()) -> Desc -> ([Line] -> [Line]) -> FilePath -> Property UnixLike
fileProperty' writer desc a f = property desc $ go =<< liftIO (doesFileExist f)
  where
	go True = do
		old <- liftIO $ readFile f
		let new = unlines (a (lines old))
		if old == new
			then noChange
			else makeChange $ updatefile new `viaStableTmp` f
	go False = makeChange $ writer f (unlines $ a [])

	-- Replicate the original file's owner and mode.
	updatefile content f' = do
		writer f' content
		s <- getFileStatus f
		setFileMode f' (fileMode s)
		setOwnerAndGroup f' (fileOwner s) (fileGroup s)

-- | Ensures a directory exists.
dirExists :: FilePath -> Property UnixLike
dirExists d = check (not <$> doesDirectoryExist d) $ property (d ++ " exists") $
	makeChange $ createDirectoryIfMissing True d

-- | The location that a symbolic link points to.
newtype LinkTarget = LinkTarget FilePath

-- | Creates or atomically updates a symbolic link.
--
-- Does not overwrite regular files or directories.
isSymlinkedTo :: FilePath -> LinkTarget -> Property UnixLike
link `isSymlinkedTo` (LinkTarget target) = property desc $
	go =<< (liftIO $ tryIO $ getSymbolicLinkStatus link)
  where
	desc = link ++ " is symlinked to " ++ target
	go (Right stat) =
		if isSymbolicLink stat
			then checkLink
			else nonSymlinkExists
	go (Left _) = makeChange $ createSymbolicLink target link

	nonSymlinkExists = do
		warningMessage $ link ++ " exists and is not a symlink"
		return FailedChange
	checkLink = do
		target' <- liftIO $ readSymbolicLink link
		if target == target'
			then noChange
			else makeChange updateLink
	updateLink = createSymbolicLink target `viaStableTmp` link

-- | Ensures that a file is a copy of another (regular) file.
isCopyOf :: FilePath -> FilePath -> Property UnixLike
f `isCopyOf` f' = property desc $ go =<< (liftIO $ tryIO $ getFileStatus f')
  where
	desc = f ++ " is copy of " ++ f'
	go (Right stat) = if isRegularFile stat
		then gocmp =<< (liftIO $ cmp)
		else warningMessage (f' ++ " is not a regular file") >>
			return FailedChange
	go (Left e) = warningMessage (show e) >> return FailedChange

	cmp = safeSystem "cmp" [Param "-s", Param "--", File f, File f']
	gocmp ExitSuccess = noChange
	gocmp (ExitFailure 1) = doit
	gocmp _ = warningMessage "cmp failed" >> return FailedChange

	doit = makeChange $ copy f' `viaStableTmp` f
	copy src dest = unlessM (runcp src dest) $ errorMessage "cp failed"
	runcp src dest = boolSystem "cp"
		[Param "--preserve=all", Param "--", File src, File dest]

-- | Ensures that a file/dir has the specified owner and group.
ownerGroup :: FilePath -> User -> Group -> Property UnixLike
ownerGroup f (User owner) (Group group) = p `describe` (f ++ " owner " ++ og)
  where
	p = cmdProperty "chown" [og, f]
		`changesFile` f
	og = owner ++ ":" ++ group

-- | Ensures that a file/dir has the specfied mode.
mode :: FilePath -> FileMode -> Property UnixLike
mode f v = p `changesFile` f
  where
	p = property (f ++ " mode " ++ show v) $ do
		liftIO $ modifyFileMode f (const v)
		return NoChange

-- | A temp file to use when writing new content for a file.
--
-- This is a stable name so it can be removed idempotently.
--
-- It ends with "~" so that programs that read many config files from a
-- directory will treat it as an editor backup file, and not read it.
stableTmpFor :: FilePath -> FilePath
stableTmpFor f = f ++ ".propellor-new~"

-- | Creates/updates a file atomically, running the action to create the
-- stable tmp file, and then renaming it into place.
viaStableTmp :: (MonadMask m, MonadIO m) => (FilePath -> m ()) -> FilePath -> m ()
viaStableTmp a f = bracketIO setup cleanup go
  where
	setup = do
		createDirectoryIfMissing True (takeDirectory f)
		let tmpfile = stableTmpFor f
		nukeFile tmpfile
		return tmpfile
	cleanup tmpfile = tryIO $ removeFile tmpfile
	go tmpfile = do
		a tmpfile
		liftIO $ rename tmpfile f

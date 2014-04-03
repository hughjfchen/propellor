module Propellor.Property.File where

import Propellor

type Line = String

-- | Replaces all the content of a file.
hasContent :: FilePath -> [Line] -> Property
f `hasContent` newcontent = fileProperty ("replace " ++ f)
	(\_oldcontent -> newcontent) f

-- | Ensures that a line is present in a file, adding it to the end if not.
containsLine :: FilePath -> Line -> Property
f `containsLine` l = fileProperty (f ++ " contains:" ++ l) go f
  where
	go ls
		| l `elem` ls = ls
		| otherwise = ls++[l]

-- | Ensures that a line is not present in a file.
-- Note that the file is ensured to exist, so if it doesn't, an empty
-- file will be written.
lacksLine :: FilePath -> Line -> Property
f `lacksLine` l = fileProperty (f ++ " remove: " ++ l) (filter (/= l)) f

-- | Removes a file. Does not remove symlinks or non-plain-files.
notPresent :: FilePath -> Property
notPresent f = check (doesFileExist f) $ Property (f ++ " not present") $ 
	makeChange $ nukeFile f

fileProperty :: Desc -> ([Line] -> [Line]) -> FilePath -> Property
fileProperty desc a f = Property desc $ go =<< doesFileExist f
  where
	go True = do
		ls <- lines <$> catchDefaultIO [] (readFile f)
		let ls' = a ls
		if ls' == ls
			then noChange
			else makeChange $ viaTmp writeFile f (unlines ls')
	go False = makeChange $ writeFile f (unlines $ a [])

-- | Ensures a directory exists.
dirExists :: FilePath -> Property
dirExists d = check (not <$> doesDirectoryExist d) $ Property (d ++ " exists") $
	makeChange $ createDirectoryIfMissing True d

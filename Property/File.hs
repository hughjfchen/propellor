module Property.File where

import System.Directory

import Property
import Utility.Directory

{- Replaces all the content of a file. -}
hasContent :: FilePath -> [Line] -> Property
f `hasContent` newcontent = FileProperty ("replace " ++ f)
	f (\_oldcontent -> newcontent)

{- Ensures that a line is present in a file, adding it to the end if not. -}
containsLine :: FilePath -> Line -> Property
f `containsLine` l = FileProperty (f ++ " contains:" ++ l) f go
  where
	go ls
		| l `elem` ls = ls
		| otherwise = ls++[l]

{- Ensures that a line is not present in a file.
 - Note that the file is ensured to exist, so if it doesn't, an empty
 - file will be written. -}
lacksLine :: FilePath -> Line -> Property
f `lacksLine` l = FileProperty (f ++ " remove: " ++ l) f (filter (/= l))

{- Note: Does not remove symlinks or non-plain-files. -}
notPresent :: FilePath -> Property
notPresent f = check (doesFileExist f) $ IOProperty (f ++ " not present") $ 
	makeChange $ nukeFile f

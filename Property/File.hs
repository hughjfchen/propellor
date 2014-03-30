module Property.File where

import Property

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

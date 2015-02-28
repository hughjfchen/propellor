module Propellor.Bootstrap (
	buildPropellor
) where

import Propellor
import Utility.SafeCommand

import System.Posix.Files

buildPropellor :: IO ()
buildPropellor = unlessM (actionMessage "Propellor build" build) $
	errorMessage "Propellor build failed!"

-- Build propellor using cabal, and symlink propellor to where cabal
-- leaves the built binary.
--
-- For speed, only runs cabal configure when it's not been run before.
-- If the build fails cabal may need to have configure re-run.
build :: IO Bool
build = catchBoolIO $ do
	make "dist/setup-config" ["propellor.cabal"] $
		cabal ["configure"]
	unlessM (cabal ["build"]) $ do
		void $ cabal ["configure"]
		unlessM (cabal ["build"]) $
			error "cabal build failed"
	nukeFile "propellor"
	createSymbolicLink "dist/build/propellor-config/propellor-config" "propellor"
	return True

make :: FilePath -> [FilePath] -> IO Bool -> IO ()
make dest srcs builder = do
	dt <- getmtime dest
	st <- mapM getmtime srcs
	when (dt == Nothing || any (> dt) st) $
		unlessM builder $
			error $ "failed to make " ++ dest
  where
	getmtime = catchMaybeIO . getModificationTime

cabal :: [String] -> IO Bool
cabal = boolSystem "cabal" . map Param

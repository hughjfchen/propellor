module Propellor.Bootstrap (
	bootstrapPropellorCommand,
	installGitCommand,
	buildPropellor,
) where

import Propellor
import Utility.SafeCommand

import System.Posix.Files
import Data.List

type ShellCommand = String

-- Shell command line to build propellor, used when bootstrapping on a new
-- host. Should be run inside the propellor config dir, and will install
-- all necessary build dependencies.
bootstrapPropellorCommand :: ShellCommand
bootstrapPropellorCommand = "if ! test -x ./propellor; then " ++ go ++ "; fi"
  where
	go = intercalate " && "
		[ depsCommand
		, buildCommand
		]

buildCommand :: ShellCommand
buildCommand = intercalate " && "
	[ "cabal configure"
	, "cabal build"
	, "ln -sf dist/build/propellor-config/propellor-config propellor"
	]

-- Install build dependencies of propellor.
--
-- First, try to install ghc, cabal, gnupg, and all haskell libraries that
-- propellor uses from OS packages.
--
-- Some packages may not be available in some versions of Debian
-- (eg, Debian wheezy lacks async), or propellor may need a newer version.
-- So, as a second step, cabal is used to install all dependencies.
--
-- Note: May succeed and leave some deps not installed.
depsCommand :: ShellCommand
depsCommand = "( " ++ intercalate " ; " (concat [osinstall, cabalinstall]) ++ " ) || true"
  where
	osinstall = "apt-get update" : map aptinstall debdeps

	cabalinstall = 
		[ "cabal update"
		, "cabal install --only-dependencies"
		]

	aptinstall p = "apt-get --no-upgrade --no-install-recommends -y install " ++ p

	-- This is the same build deps listed in debian/control.
	debdeps =
		[ "gnupg"
		, "ghc"
		, "cabal-install"
		, "libghc-async-dev"
		, "libghc-missingh-dev"
		, "libghc-hslogger-dev"
		, "libghc-unix-compat-dev"
		, "libghc-ansi-terminal-dev"
		, "libghc-ifelse-dev"
		, "libghc-network-dev"
		, "libghc-quickcheck2-dev"
		, "libghc-mtl-dev"
		, "libghc-monadcatchio-transformers-dev"
		]

installGitCommand :: ShellCommand
installGitCommand = "if ! git --version >/dev/null; then apt-get update && apt-get --no-install-recommends --no-upgrade -y install git; fi"

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

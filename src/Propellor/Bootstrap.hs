module Propellor.Bootstrap (
	bootstrapPropellorCommand,
	checkBinaryCommand,
	installGitCommand,
	buildPropellor,
) where

import Propellor.Base

import System.Posix.Files
import Data.List

type ShellCommand = String

-- Shell command line to ensure propellor is bootstrapped and ready to run.
-- Should be run inside the propellor config dir, and will install
-- all necessary build dependencies and build propellor.
bootstrapPropellorCommand :: ShellCommand
bootstrapPropellorCommand = checkDepsCommand ++ 
	"&& if ! test -x ./propellor; then " 
		++ buildCommand ++ 
	"; fi;" ++ checkBinaryCommand

-- Use propellor --check to detect if the local propellor binary has
-- stopped working (eg due to library changes), and must be rebuilt.
checkBinaryCommand :: ShellCommand
checkBinaryCommand = "if test -x ./propellor && ! ./propellor --check 2>/dev/null; then " ++ go ++ "; fi"
  where
	go = intercalate " && "
		[ "cabal clean"
		, buildCommand
		]

buildCommand :: ShellCommand
buildCommand = intercalate " && "
	[ "cabal configure"
	, "cabal build"
	, "ln -sf dist/build/propellor-config/propellor-config propellor"
	]

-- Run cabal configure to check if all dependencies are installed;
-- if not, run the depsCommand.
checkDepsCommand :: ShellCommand
checkDepsCommand = "if ! cabal configure >/dev/null 2>&1; then " ++ depsCommand ++ "; fi"

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

	aptinstall p = "DEBIAN_FRONTEND=noninteractive apt-get --no-upgrade --no-install-recommends -y install " ++ p

	-- This is the same deps listed in debian/control.
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
		, "libghc-mtl-dev"
		, "libghc-transformers-dev"
		, "libghc-exceptions-dev"
		, "libghc-stm-dev"
		, "libghc-text-dev"
		, "make"
		]

installGitCommand :: ShellCommand
installGitCommand = "if ! git --version >/dev/null; then apt-get update && DEBIAN_FRONTEND=noninteractive apt-get --no-install-recommends --no-upgrade -y install git; fi"

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
	-- For safety against eg power loss in the middle of the build,
	-- make a copy of the binary, and move it into place atomically.
	-- This ensures that the propellor symlink only ever points at
	-- a binary that is fully built. Also, avoid ever removing
	-- or breaking the symlink.
	--
	-- Need cp -a to make build timestamp checking work.
	unlessM (boolSystem "cp" [Param "-af", Param cabalbuiltbin, Param (tmpfor safetycopy)]) $
		error "cp of binary failed"
	rename (tmpfor safetycopy) safetycopy
	createSymbolicLink safetycopy (tmpfor dest)
	rename (tmpfor dest) dest
	return True
  where
	dest = "propellor"
	cabalbuiltbin = "dist/build/propellor-config/propellor-config"
	safetycopy = cabalbuiltbin ++ ".built"
	tmpfor f = f ++ ".propellortmp"

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

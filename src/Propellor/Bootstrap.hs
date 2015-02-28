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
-- host. Should be run inside the propellor source tree, and will install
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

depsCommand :: ShellCommand
depsCommand = 
	"(" ++ aptinstall debdeps ++ " || (apt-get update && " ++ aptinstall debdeps ++ ")) && "
	++ "(" ++ aptinstall ["libghc-async-dev"] ++ " || (" ++ cabalinstall ["async"] ++  ")) || "
	++ "(" ++ cabalinstall ["--only-dependencies"] ++ ")"
  where
	aptinstall ps = "apt-get --no-upgrade --no-install-recommends -y install " ++ unwords ps

	cabalinstall ps = "cabal update; cabal install " ++ unwords ps

	-- This is the same build deps listed in debian/control.
	debdeps =
		[ "gnupg"
		, "ghc"
		, "cabal-install"
		-- async is not available in debian stable
		-- , "libghc-async-dev"
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

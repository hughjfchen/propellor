module Propellor.Bootstrap (
	bootstrapPropellorCommand,
	checkBinaryCommand,
	installGitCommand,
	buildPropellor,
) where

import Propellor.Base
import Propellor.Types.Info
import Propellor.Git.Config

import System.Posix.Files
import Data.List

type ShellCommand = String

-- Shell command line to ensure propellor is bootstrapped and ready to run.
-- Should be run inside the propellor config dir, and will install
-- all necessary build dependencies and build propellor.
bootstrapPropellorCommand :: Maybe System -> ShellCommand
bootstrapPropellorCommand msys = checkDepsCommand msys ++
	"&& if ! test -x ./propellor; then "
		++ buildCommand ++
	"; fi;" ++ checkBinaryCommand

-- Use propellor --check to detect if the local propellor binary has
-- stopped working (eg due to library changes), and must be rebuilt.
checkBinaryCommand :: ShellCommand
checkBinaryCommand = "if test -x ./propellor && ! ./propellor --check; then " ++ go ++ "; fi"
  where
	go = intercalate " && "
		[ "cabal clean"
		, buildCommand
		]

buildCommand :: ShellCommand
buildCommand = intercalate " && "
	[ "cabal configure"
	, "cabal build propellor-config"
	, "ln -sf dist/build/propellor-config/propellor-config propellor"
	]

-- Run cabal configure to check if all dependencies are installed;
-- if not, run the depsCommand.
checkDepsCommand :: Maybe System -> ShellCommand
checkDepsCommand sys = "if ! cabal configure >/dev/null 2>&1; then " ++ depsCommand sys ++ "; fi"

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
depsCommand :: Maybe System -> ShellCommand
depsCommand msys = "( " ++ intercalate " ; " (concat [osinstall, cabalinstall]) ++ " ) || true"
  where
	osinstall = case msys of
		Just (System (FreeBSD _) _) -> map pkginstall fbsddeps
		Just (System (Debian _ _) _) -> useapt
		Just (System (Buntish _) _) -> useapt
		-- assume a debian derived system when not specified
		Nothing -> useapt

	useapt = "apt-get update" : map aptinstall debdeps

	cabalinstall =
		[ "cabal update"
		, "cabal install --only-dependencies"
		]

	aptinstall p = "DEBIAN_FRONTEND=noninteractive apt-get -qq --no-upgrade --no-install-recommends -y install " ++ p
	pkginstall p = "ASSUME_ALWAYS_YES=yes pkg install " ++ p

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
	fbsddeps =
		[ "gnupg"
		, "ghc"
		, "hs-cabal-install"
		, "hs-async"
		, "hs-MissingH"
		, "hs-hslogger"
		, "hs-unix-compat"
		, "hs-ansi-terminal"
		, "hs-IfElse"
		, "hs-network"
		, "hs-mtl"
		, "hs-transformers-base"
		, "hs-exceptions"
		, "hs-stm"
		, "hs-text"
		, "gmake"
		]

installGitCommand :: Maybe System -> ShellCommand
installGitCommand msys = case msys of
	(Just (System (Debian _ _) _)) -> use apt
	(Just (System (Buntish _) _)) -> use apt
	(Just (System (FreeBSD _) _)) -> use
		[ "ASSUME_ALWAYS_YES=yes pkg update"
		, "ASSUME_ALWAYS_YES=yes pkg install git"
		]
	-- assume a debian derived system when not specified
	Nothing -> use apt
  where
	use cmds = "if ! git --version >/dev/null; then " ++ intercalate " && " cmds ++ "; fi"
	apt =
		[ "apt-get update"
		, "DEBIAN_FRONTEND=noninteractive apt-get -qq --no-install-recommends --no-upgrade -y install git"
		]

buildPropellor :: Maybe Host -> IO ()
buildPropellor mh = unlessM (actionMessage "Propellor build" (build msys)) $
	errorMessage "Propellor build failed!"
  where
	msys = case fmap (fromInfo . hostInfo) mh of
		Just (InfoVal sys) -> Just sys
		_ -> Nothing

-- Build propellor using cabal or stack, and symlink propellor to the
-- built binary.
build :: Maybe System -> IO Bool
build msys = catchBoolIO $ do
	bs <- getGitConfigValue "propellor.buildsystem"
	case bs of
		Just "stack" -> stackBuild msys
		_ -> cabalBuild msys

-- For speed, only runs cabal configure when it's not been run before.
-- If the build fails cabal may need to have configure re-run.
--
-- If the cabal configure fails, and a System is provided, installs
-- dependencies and retries.
cabalBuild :: Maybe System -> IO Bool
cabalBuild msys = do
	make "dist/setup-config" ["propellor.cabal"] cabal_configure
	unlessM cabal_build $
		unlessM (cabal_configure <&&> cabal_build) $
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
	symlinkPropellorBin safetycopy
	return True
  where
	cabalbuiltbin = "dist/build/propellor-config/propellor-config"
	safetycopy = cabalbuiltbin ++ ".built"
	cabal_configure = ifM (cabal ["configure"])
		( return True
		, case msys of
			Nothing -> return False
			Just sys ->
				boolSystem "sh" [Param "-c", Param (depsCommand (Just sys))]
					<&&> cabal ["configure"]
		)
	cabal_build = cabal ["build", "propellor-config"]

stackBuild :: Maybe System -> IO Bool
stackBuild _msys = do
	createDirectoryIfMissing True builddest
	ifM (stack buildparams)
		( do
			symlinkPropellorBin (builddest </> "propellor-config")
			return True
		, return False
		)
  where
 	builddest = ".built"
	buildparams =
		[ "--local-bin-path", builddest
		, "build"
		, ":propellor-config" -- only build config program
		, "--copy-bins"
		]

-- Atomic symlink creation/update.
symlinkPropellorBin :: FilePath -> IO ()
symlinkPropellorBin bin = do
	createSymbolicLink bin (tmpfor dest)
	rename (tmpfor dest) dest
  where
	dest = "propellor"

tmpfor :: FilePath -> FilePath
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

stack :: [String] -> IO Bool
stack = boolSystem "stack" . map Param

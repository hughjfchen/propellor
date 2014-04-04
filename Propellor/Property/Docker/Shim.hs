-- | Support for running propellor, as built outside a docker container,
-- inside the container.
--
-- Note: This is currently Debian specific, due to glibcLibs.

module Propellor.Property.Docker.Shim (setup, file) where

import Propellor
import Utility.LinuxMkLibs
import Utility.SafeCommand
import Utility.Path
import Utility.FileMode

import Data.List
import System.Posix.Files

-- | Sets up a shimmed version of the program, in a directory, and
-- returns its path.
setup :: FilePath -> FilePath -> IO FilePath
setup propellorbin dest = do
	createDirectoryIfMissing True dest

	libs <- parseLdd <$> readProcess "ldd" [propellorbin]
	glibclibs <- glibcLibs
	let libs' = nub $ libs ++ glibclibs
	libdirs <- map (dest ++) . nub . catMaybes
		<$> mapM (installLib installFile dest) libs'
	
	let linker = (dest ++) $ 
		fromMaybe (error "cannot find ld-linux linker") $
			headMaybe $ filter ("ld-linux" `isInfixOf`) libs'
	let linkerparams = ["--library-path", intercalate ":" libdirs ]
	let shim = file propellorbin dest
	writeFile shim $ unlines
		[ "#!/bin/sh"
		, "exec " ++ unwords (map shellEscape $ linker : linkerparams) ++ 
			" " ++ shellEscape propellorbin ++ " \"$@\""
		]
	modifyFileMode shim (addModes executeModes)
	return shim

file :: FilePath -> FilePath -> FilePath
file propellorbin dest = dest </> propellorbin

installFile :: FilePath -> FilePath -> IO ()
installFile top f = do
	createDirectoryIfMissing True destdir
	createLink f dest `catchIO` (const copy)
  where
	copy = void $ boolSystem "cp" [Param "-a", Param f, Param dest]
	destdir = inTop top $ parentDir f
	dest = inTop top f

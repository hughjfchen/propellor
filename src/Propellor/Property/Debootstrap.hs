{-# LANGUAGE FlexibleContexts #-}

module Propellor.Property.Debootstrap (
	Url,
	DebootstrapConfig(..),
	built,
	built',
	installed,
	sourceInstall,
	programPath,
) where

import Propellor.Base
import qualified Propellor.Property.Apt as Apt
import Propellor.Property.Chroot.Util
import Utility.Path
import Utility.FileMode

import Data.List
import Data.Char
import System.Posix.Directory
import System.Posix.Files

type Url = String

-- | A monoid for debootstrap configuration. 
-- mempty is a default debootstrapped system.
data DebootstrapConfig
	= DefaultConfig
	| MinBase
	| BuilddD
	| DebootstrapParam String
	| DebootstrapConfig :+ DebootstrapConfig
	deriving (Show)

instance Monoid DebootstrapConfig where
        mempty  = DefaultConfig
        mappend = (:+)

toParams :: DebootstrapConfig -> [CommandParam]
toParams DefaultConfig = []
toParams MinBase = [Param "--variant=minbase"]
toParams BuilddD = [Param "--variant=buildd"]
toParams (DebootstrapParam p) = [Param p]
toParams (c1 :+ c2) = toParams c1 <> toParams c2

-- | Builds a chroot in the given directory using debootstrap.
--
-- The System can be any OS and architecture that debootstrap
-- and the kernel support.
built :: FilePath -> System -> DebootstrapConfig -> Property HasInfo
built target system config = built' (toProp installed) target system config

built' :: (Combines (Property NoInfo) (Property i)) => Property i -> FilePath -> System -> DebootstrapConfig -> Property (CInfo NoInfo i)
built' installprop target system@(System _ arch) config = 
	check (unpopulated target <||> ispartial) setupprop
		`requires` installprop
  where
	setupprop = property ("debootstrapped " ++ target) $ liftIO $ do
		createDirectoryIfMissing True target
		-- Don't allow non-root users to see inside the chroot,
		-- since doing so can allow them to do various attacks
		-- including hard link farming suid programs for later
		-- exploitation.
		modifyFileMode target (removeModes [otherReadMode, otherExecuteMode, otherWriteMode])
		suite <- case extractSuite system of
			Nothing -> errorMessage $ "don't know how to debootstrap " ++ show system
			Just s -> pure s
		let params = toParams config ++
			[ Param $ "--arch=" ++ arch
			, Param suite
			, Param target
			]
		cmd <- fromMaybe "debootstrap" <$> programPath
		de <- standardPathEnv
		ifM (boolSystemEnv cmd params (Just de))
			( do
				fixForeignDev target
				return MadeChange
			, return FailedChange
			)

	-- A failed debootstrap run will leave a debootstrap directory;
	-- recover by deleting it and trying again.
	ispartial = ifM (doesDirectoryExist (target </> "debootstrap"))
		( do
			removeChroot target
			return True
		, return False
		)
	
extractSuite :: System -> Maybe String
extractSuite (System (Debian s) _) = Just $ Apt.showSuite s
extractSuite (System (Buntish r) _) = Just r

-- | Ensures debootstrap is installed.
--
-- When necessary, falls back to installing debootstrap from source.
-- Note that installation from source is done by downloading the tarball
-- from a Debian mirror, with no cryptographic verification.
installed :: RevertableProperty NoInfo
installed = install <!> remove
  where
	install = withOS "debootstrap installed" $ \o -> 
		ifM (liftIO $ isJust <$> programPath)
			( return NoChange
			, ensureProperty (installon o)
			)

	installon (Just (System (Debian _) _)) = aptinstall
	installon (Just (System (Buntish _) _)) = aptinstall
	installon _ = sourceInstall

	remove = withOS "debootstrap removed" $ ensureProperty . removefrom
	removefrom (Just (System (Debian _) _)) = aptremove
	removefrom (Just (System (Buntish _) _)) = aptremove
	removefrom _ = sourceRemove
			
	aptinstall = Apt.installed ["debootstrap"]
	aptremove = Apt.removed ["debootstrap"]

sourceInstall :: Property NoInfo
sourceInstall = property "debootstrap installed from source" (liftIO sourceInstall')
	`requires` perlInstalled
	`requires` arInstalled

perlInstalled :: Property NoInfo
perlInstalled = check (not <$> inPath "perl") $ property "perl installed" $
	liftIO $ toResult . isJust <$> firstM id
		[ yumInstall "perl"
		]

arInstalled :: Property NoInfo
arInstalled = check (not <$> inPath "ar") $ property "ar installed" $
	liftIO $ toResult . isJust <$> firstM id
		[ yumInstall "binutils"
		]

yumInstall :: String -> IO Bool
yumInstall p = boolSystem "yum" [Param "-y", Param "install", Param p]

sourceInstall' :: IO Result
sourceInstall' = withTmpDir "debootstrap" $ \tmpd -> do
	let indexfile = tmpd </> "index.html"
	unlessM (download baseurl indexfile) $
		errorMessage $ "Failed to download " ++ baseurl
	urls <- sortBy (flip compare) -- highest version first
		. filter ("debootstrap_" `isInfixOf`)
		. filter (".tar." `isInfixOf`)
		. extractUrls baseurl <$>
		readFileStrictAnyEncoding indexfile
	nukeFile indexfile

	tarfile <- case urls of
		(tarurl:_) -> do
			let f = tmpd </> takeFileName tarurl
			unlessM (download tarurl f) $
				errorMessage $ "Failed to download " ++ tarurl
			return f
		_ -> errorMessage $ "Failed to find any debootstrap tarballs listed on " ++ baseurl

	createDirectoryIfMissing True localInstallDir
	bracket getWorkingDirectory changeWorkingDirectory $ \_ -> do
		changeWorkingDirectory localInstallDir
		unlessM (boolSystem "tar" [Param "xf", File tarfile]) $
			errorMessage "Failed to extract debootstrap tar file"
		nukeFile tarfile
		l <- dirContents "."
		case l of
			(subdir:[]) -> do
				changeWorkingDirectory subdir
				makeDevicesTarball
				makeWrapperScript (localInstallDir </> subdir)
				return MadeChange
			_ -> errorMessage "debootstrap tar file did not contain exactly one dirctory"

sourceRemove :: Property NoInfo
sourceRemove = property "debootstrap not installed from source" $ liftIO $
	ifM (doesDirectoryExist sourceInstallDir)
		( do
			removeDirectoryRecursive sourceInstallDir
			return MadeChange
		, return NoChange
		)

sourceInstallDir :: FilePath
sourceInstallDir = "/usr/local/propellor/debootstrap"

wrapperScript :: FilePath
wrapperScript = sourceInstallDir </> "debootstrap.wrapper"

-- | Finds debootstrap in PATH, but fall back to looking for the
-- wrapper script that is installed, outside the PATH, when debootstrap
-- is installed from source.
programPath :: IO (Maybe FilePath)
programPath = getM searchPath
	[ "debootstrap"
	, wrapperScript
	]

makeWrapperScript :: FilePath -> IO ()
makeWrapperScript dir = do
	createDirectoryIfMissing True (takeDirectory wrapperScript)
	writeFile wrapperScript $ unlines
		[ "#!/bin/sh"
		, "set -e"
		, "DEBOOTSTRAP_DIR=" ++ dir
		, "export DEBOOTSTRAP_DIR"
		, dir </> "debootstrap" ++ " \"$@\""
		]
	modifyFileMode wrapperScript (addModes $ readModes ++ executeModes)

-- Work around for <http://bugs.debian.org/770217>
makeDevicesTarball :: IO ()
makeDevicesTarball = do
	-- TODO append to tarball; avoid writing to /dev
	writeFile foreignDevFlag "1"
	ok <- boolSystem "sh" [Param "-c", Param tarcmd]
	nukeFile foreignDevFlag
	unless ok $
		errorMessage "Failed to tar up /dev to generate devices.tar.gz"
  where
	tarcmd = "(cd / && tar cf - dev) | gzip > devices.tar.gz"

fixForeignDev :: FilePath -> IO ()
fixForeignDev target = whenM (doesFileExist (target ++ foreignDevFlag)) $ do
	de <- standardPathEnv
	void $ boolSystemEnv "chroot"
		[ File target
		, Param "sh"
		, Param "-c"
		, Param $ intercalate " && "
			[ "apt-get update"
			, "apt-get -y install makedev"
			, "rm -rf /dev"
			, "mkdir /dev"
			, "cd /dev"
			, "mount -t proc proc /proc"
			, "/sbin/MAKEDEV std ptmx fd consoleonly"
			]
		]
		(Just de)

foreignDevFlag :: FilePath
foreignDevFlag = "/dev/.propellor-foreign-dev"

localInstallDir :: FilePath
localInstallDir = "/usr/local/debootstrap"

-- This http server directory listing is relied on to be fairly sane,
-- which is one reason why it's using a specific server and not a
-- round-robin address.
baseurl :: Url
baseurl = "http://ftp.debian.org/debian/pool/main/d/debootstrap/"

download :: Url -> FilePath -> IO Bool
download url dest = anyM id
	[ boolSystem "curl" [Param "-o", File dest, Param url]
	, boolSystem "wget" [Param "-O", File dest, Param url]
	]

-- Pretty hackish, but I don't want to pull in a whole html parser
-- or parsec dependency just for this.
--
-- To simplify parsing, lower case everything. This is ok because
-- the filenames are all lower-case anyway.
extractUrls :: Url -> String -> [Url]
extractUrls base = collect [] . map toLower
  where
	collect l [] = l
	collect l ('h':'r':'e':'f':'=':r) = case r of
		('"':r') -> findend l r'
		_ -> findend l r
	collect l (_:cs) = collect l cs

	findend l s = 
		let (u, r) = break (== '"') s
		    u' = if "http" `isPrefixOf` u
		    	then u
			else base </> u
		in collect (u':l) r

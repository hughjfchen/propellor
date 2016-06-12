{-# LANGUAGE OverloadedStrings #-}

module Propellor.Property.Reboot (
	now,
	atEnd,
	toDistroKernel,
	toKernelNewerThan,
) where

import Propellor.Base

import Data.List
import Data.Version
import Text.ParserCombinators.ReadP

type KernelVersion = String

now :: Property Linux
now = tightenTargets $ cmdProperty "reboot" []
	`assume` MadeChange
	`describe` "reboot now"

-- | Schedules a reboot at the end of the current propellor run.
--
-- The `Result` code of the entire propellor run can be checked;
-- the reboot proceeds only if the function returns True.
--
-- The reboot can be forced to run, which bypasses the init system. Useful
-- if the init system might not be running for some reason.
atEnd :: Bool -> (Result -> Bool) -> Property Linux
atEnd force resultok = property "scheduled reboot at end of propellor run" $ do
	endAction "rebooting" atend
	return NoChange
  where
	atend r
		| resultok r = liftIO $ toResult
			<$> boolSystem "reboot" rebootparams
		| otherwise = do
			warningMessage "Not rebooting, due to status of propellor run."
			return FailedChange
	rebootparams
		| force = [Param "--force"]
		| otherwise = []

-- | Reboots immediately if a kernel other than the distro-installed kernel is
-- running.
--
-- This will only work if you have taken measures to ensure that the other
-- kernel won't just get booted again.  See 'Propellor.Property.DigitalOcean'
-- for an example.
toDistroKernel :: Property DebianLike
toDistroKernel = check (not <$> runningInstalledKernel) now
	`describe` "running installed kernel"

-- | Given a kernel version string @v@, reboots immediately if the running
-- kernel version is strictly less than @v@ and there is an installed kernel
-- version is greater than or equal to @v@
--
-- This assumes that the installed kernel with the highest version number is the
-- one that will be started if we reboot.
--
-- This is useful when upgrading to a new version of Debian where you need to
-- ensure that a new enough kernel is running before ensuring other properties.
toKernelNewerThan :: KernelVersion -> Property DebianLike
toKernelNewerThan ver =
	property' ("reboot to kernel newer than " ++ ver) $ \w -> do
		let wantV = readVersion ver
		runningV <- readVersion <$> liftIO runningKernelVersion
		installedV <- maximum . map readVersion <$>
			liftIO installedKernelVersions
		if runningV >= wantV then noChange
			else if installedV >= wantV && runningV < wantV
				then ensureProperty w now
				else error "newer kernel not installed"
					>> return FailedChange

runningInstalledKernel :: IO Bool
runningInstalledKernel = do
	kernelver <- runningKernelVersion
	when (null kernelver) $
		error "failed to read uname -r"
	kernelimages <- installedKernelImages
	when (null kernelimages) $
		error "failed to find any installed kernel images"
	findVersion kernelver <$>
		readProcess "file" ("-L" : kernelimages)

runningKernelVersion :: IO KernelVersion
runningKernelVersion = takeWhile (/= '\n') <$> readProcess "uname" ["-r"]

installedKernelImages :: IO [String]
installedKernelImages = concat <$> mapM kernelsIn ["/", "/boot/"]

-- | File output looks something like this, we want to unambiguously
-- match the running kernel version:
--   Linux kernel x86 boot executable bzImage, version 3.16-3-amd64 (debian-kernel@lists.debian.org) #1 SMP Debian 3.1, RO-rootFS, swap_dev 0x2, Normal VGA
findVersion :: KernelVersion -> String -> Bool
findVersion ver s = (" version " ++ ver ++ " ") `isInfixOf` s

installedKernelVersions :: IO [KernelVersion]
installedKernelVersions = do
	kernelimages <- installedKernelImages
	when (null kernelimages) $
		error "failed to find any installed kernel images"
	imageLines <- lines <$> readProcess "file" ("-L" : kernelimages)
	return $ extractKernelVersion <$> imageLines

kernelsIn :: FilePath -> IO [FilePath]
kernelsIn d = filter ("vmlinu" `isInfixOf`) <$> dirContents d

extractKernelVersion :: String -> KernelVersion
extractKernelVersion =
	unwords . take 1 . drop 1 . dropWhile (/= "version") . words

-- TODO properly handle error here
readVersion :: String -> Version
readVersion s = (fst . Prelude.last) $ readP_to_S parseVersion s

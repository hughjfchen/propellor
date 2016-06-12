module Propellor.Property.Reboot (
	now,
	atEnd,
	toDistroKernel,
	toKernelNewerThan,
) where

import Propellor.Base

import Data.List

data Version = String

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
-- kernel version is strictly less than @v@ and the installed kernel version is
-- greater than or equal to @v@
--
-- This is useful when upgrading to a new version of Debian where you need to
-- ensure that a new enough kernel is running before ensuring other properties.
toKernelNewerThan :: Version -> Property DebianLike
toKernelNewerThan v = undefined

runningInstalledKernel :: IO Bool
runningInstalledKernel = do
	kernelver <- takeWhile (/= '\n') <$> readProcess "uname" ["-r"]
	when (null kernelver) $
		error "failed to read uname -r"
	kernelimages <- concat <$> mapM kernelsIn ["/", "/boot/"]
	when (null kernelimages) $
		error "failed to find any installed kernel images"
	findVersion kernelver <$>
		readProcess "file" ("-L" : kernelimages)

-- | File output looks something like this, we want to unambiguously
-- match the running kernel version:
--   Linux kernel x86 boot executable bzImage, version 3.16-3-amd64 (debian-kernel@lists.debian.org) #1 SMP Debian 3.1, RO-rootFS, swap_dev 0x2, Normal VGA
findVersion :: String -> String -> Bool
findVersion ver s = (" version " ++ ver ++ " ") `isInfixOf` s

kernelsIn :: FilePath -> IO [FilePath]
kernelsIn d = filter ("vmlinu" `isInfixOf`) <$> dirContents d

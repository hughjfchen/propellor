module Propellor.Property.Bootstrap (RepoSource(..), bootstrappedFrom, clonedFrom) where

import Propellor.Base
import Propellor.Bootstrap
import Propellor.Property.Chroot

import Data.List
import qualified Data.ByteString as B

-- | Where a propellor repository should be bootstrapped from.
data RepoSource
	= GitRepoUrl String
	| GitRepoOutsideChroot
	-- ^ When used in a chroot, this copies the git repository from
	-- outside the chroot, including its configuration.

-- | Bootstraps a propellor installation into
-- /usr/local/propellor/
--
-- Normally, propellor is already bootstrapped when it runs, so this
-- property is not useful. However, this can be useful inside a
-- chroot used to build a disk image, to make the disk image
-- have propellor installed.
--
-- The git repository is cloned (or pulled to update if it already exists).
--
-- All build dependencies are installed, using distribution packages
-- or falling back to using cabal.
bootstrappedFrom :: RepoSource -> Property Linux
bootstrappedFrom reposource = go `requires` clonedFrom reposource
  where
	go :: Property Linux
	go = property "Propellor bootstrapped" $ do
		system <- getOS
		assumeChange $ exposeTrueLocaldir $ const $ 
			runShellCommand $ buildShellCommand
				[ "cd " ++ localdir
				, bootstrapPropellorCommand system
				]

-- | Clones the propellor repeository into /usr/local/propellor/
--
-- If the propellor repo has already been cloned, pulls to get it
-- up-to-date.
clonedFrom :: RepoSource -> Property Linux
clonedFrom reposource = case reposource of
	GitRepoOutsideChroot -> go `onChange` copygitconfig
	_ -> go
  where
	go :: Property Linux
	go = property ("Propellor repo cloned from " ++ sourcedesc) $
		ifM needclone (makeclone, updateclone)
	
	makeclone = do
		let tmpclone = localdir ++ ".tmpclone"
		system <- getOS
		assumeChange $ exposeTrueLocaldir $ \sysdir -> do
			let originloc = case reposource of
				GitRepoUrl s -> s
				GitRepoOutsideChroot -> sysdir
			runShellCommand $ buildShellCommand
				[ installGitCommand system
				, "rm -rf " ++ tmpclone
				, "git clone " ++ shellEscape originloc ++ " " ++ tmpclone
				, "mkdir -p " ++ localdir
				-- This is done rather than deleting
				-- the old localdir, because if it is bound
				-- mounted from outside the chroot, deleting
				-- it after unmounting in unshare will remove
				-- the bind mount outside the unshare.
				, "(cd " ++ tmpclone ++ " && tar c .) | (cd " ++ localdir ++ " && tar x)"
				, "rm -rf " ++ tmpclone
				]
	
	updateclone = assumeChange $ exposeTrueLocaldir $ const $
		runShellCommand $ buildShellCommand
			[ "cd " ++ localdir
			, "git pull"
			]
	
	-- Copy the git config of the repo outside the chroot into the
	-- chroot. This way it has the same remote urls, and other git
	-- configuration.
	copygitconfig :: Property Linux
	copygitconfig = property ("Propellor repo git config copied from outside the chroot") $ do
		let gitconfig = localdir </> ".git" </> "config"
		cfg <- liftIO $ B.readFile gitconfig
		exposeTrueLocaldir $ const $
			liftIO $ B.writeFile gitconfig cfg
		return MadeChange

	needclone = (inChroot <&&> truelocaldirisempty)
		<||> (liftIO (not <$> doesDirectoryExist localdir))
	
	truelocaldirisempty = exposeTrueLocaldir $ const $
		runShellCommand ("test ! -d " ++ localdir ++ "/.git")

	sourcedesc = case reposource of
		GitRepoUrl s -> s
		GitRepoOutsideChroot -> localdir ++ " outside the chroot"

assumeChange :: Propellor Bool -> Propellor Result
assumeChange a = do
	ok <- a
	return (cmdResult ok <> MadeChange)

buildShellCommand :: [String] -> String
buildShellCommand = intercalate "&&" . map (\c -> "(" ++ c ++ ")")

runShellCommand :: String -> Propellor Bool
runShellCommand s = liftIO $ boolSystem "sh" [ Param "-c", Param s]

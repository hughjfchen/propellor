module Propellor.Git.VerifiedBranch where

import Propellor.Base
import Propellor.Git
import Propellor.PrivData.Paths
import Utility.FileMode

{- To verify origin branch commit's signature, have to convince gpg
 - to use our keyring.
 - While running git log. Which has no way to pass options to gpg.
 - Argh!
 -}
verifyOriginBranch :: String -> IO Bool
verifyOriginBranch originbranch = do
	let gpgconf = privDataDir </> "gpg.conf"
	keyring <- privDataKeyring
	writeFile gpgconf $ unlines
		[ " keyring " ++ keyring
		, "no-auto-check-trustdb"
		]
	-- gpg is picky about perms
	modifyFileMode privDataDir (removeModes otherGroupModes)
	s <- readProcessEnv "git" ["log", "-n", "1", "--format=%G?", originbranch]
		(Just [("GNUPGHOME", privDataDir)])
	nukeFile $ privDataDir </> "trustdb.gpg"
	nukeFile $ privDataDir </> "pubring.gpg"
	nukeFile $ privDataDir </> "gpg.conf"
	return (s == "U\n" || s == "G\n")

-- Returns True if HEAD is changed by fetching and merging from origin.
fetchOrigin :: IO Bool
fetchOrigin = do
	branchref <- getCurrentBranch
	let originbranch = "origin" </> branchref

	void $ actionMessage "Pull from central git repository" $
		boolSystem "git" [Param "fetch"]

	oldsha <- getCurrentGitSha1 branchref

	keyring <- privDataKeyring
	whenM (doesFileExist keyring) $
		ifM (verifyOriginBranch originbranch)
			( do
				putStrLn $ "git branch " ++ originbranch ++ " gpg signature verified; merging"
				hFlush stdout
				void $ boolSystem "git" [Param "merge", Param originbranch]
			, warningMessage $ "git branch " ++ originbranch ++ " is not signed with a trusted gpg key; refusing to deploy it! (Running with previous configuration instead.)"
			)

	newsha <- getCurrentGitSha1 branchref
	return $ oldsha /= newsha

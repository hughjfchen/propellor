module Propellor.Git where

import Propellor
import Propellor.PrivData.Paths
import Propellor.Gpg
import Utility.FileMode

getCurrentBranch :: IO String
getCurrentBranch = takeWhile (/= '\n') 
	<$> readProcess "git" ["symbolic-ref", "--short", "HEAD"]

getCurrentBranchRef :: IO String
getCurrentBranchRef = takeWhile (/= '\n') 
	<$> readProcess "git" ["symbolic-ref", "HEAD"]

getCurrentGitSha1 :: String -> IO String
getCurrentGitSha1 branchref = takeWhile (/= '\n')
	<$> readProcess "git" ["show-ref", "--hash", branchref]

setRepoUrl :: String -> IO ()
setRepoUrl "" = return ()
setRepoUrl url = do
	subcmd <- ifM hasOrigin (pure "set-url", pure "add")
	void $ boolSystem "git" [Param "remote", Param subcmd, Param "origin", Param url]
	-- same as --set-upstream-to, except origin branch
	-- may not have been pulled yet
	branch <- getCurrentBranch
	let branchval s = "branch." ++ branch ++ "." ++ s
	void $ boolSystem "git" [Param "config", Param (branchval "remote"), Param "origin"]
	void $ boolSystem "git" [Param "config", Param (branchval "merge"), Param $ "refs/heads/"++branch]

getRepoUrl :: IO (Maybe String)
getRepoUrl = getM get urls
  where
	urls = ["remote.deploy.url", "remote.origin.url"]
	get u = do
		v <- catchMaybeIO $ 
			takeWhile (/= '\n') 
				<$> readProcess "git" ["config", u]
		return $ case v of
			Just url | not (null url) -> Just url
			_ -> Nothing

hasOrigin :: IO Bool
hasOrigin = catchDefaultIO False $ do
	rs <- lines <$> readProcess "git" ["remote"]
	return $ "origin" `elem` rs

hasGitRepo :: IO Bool
hasGitRepo = doesFileExist ".git/HEAD"

{- To verify origin branch commit's signature, have to convince gpg
 - to use our keyring.
 - While running git log. Which has no way to pass options to gpg.
 - Argh!
 -}
verifyOriginBranch :: String -> IO Bool
verifyOriginBranch originbranch = do
	let gpgconf = privDataDir </> "gpg.conf"
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

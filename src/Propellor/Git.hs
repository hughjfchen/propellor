module Propellor.Git where

import Propellor
import Propellor.PrivData.Paths
import Propellor.Gpg
import Utility.SafeCommand
import Utility.FileMode

getCurrentBranch :: IO String
getCurrentBranch = takeWhile (/= '\n') 
	<$> readProcess "git" ["symbolic-ref", "--short", "HEAD"]

getCurrentGitSha1 :: String -> IO String
getCurrentGitSha1 branchref = readProcess "git" ["show-ref", "--hash", branchref]

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
hasOrigin = do
	rs <- lines <$> readProcess "git" ["remote"]
	return $ "origin" `elem` rs

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

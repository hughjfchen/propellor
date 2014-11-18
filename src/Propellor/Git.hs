module Propellor.Git where

import Propellor
import Utility.SafeCommand

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


module CmdLine where

import System.Environment
import Data.List
import System.Exit

import Common
import Utility.FileMode

data CmdLine
	= Run HostName
	| Spin HostName
	| Boot HostName
	| Set HostName PrivDataField String

processCmdLine :: IO CmdLine
processCmdLine = go =<< getArgs
  where
  	go ("--help":_) = usage
  	go ("--spin":h:[]) = return $ Spin h
  	go ("--boot":h:[]) = return $ Boot h
	go ("--set":h:f:v:[]) = case readish f of
		Just pf -> return $ Set h pf v
		Nothing -> error $ "Unknown privdata field " ++ f
	go (h:[]) = return $ Run h
	go [] = do
		s <- takeWhile (/= '\n') <$> readProcess "hostname" ["-f"]
		if null s
			then error "Cannot determine hostname! Pass it on the command line."
			else return $ Run s
	go _ = usage
	
usage :: IO a
usage = do
	putStrLn $ unlines 
		[ "Usage:"
		, "  propellor"
		, "  propellor hostname"
		, "  propellor --spin hostname"
		, "  propellor --set hostname field value"
		]
	exitFailure

defaultMain :: (HostName -> [Property]) -> IO ()
defaultMain getprops = go =<< processCmdLine
  where
	go (Run host) = ensureProperties (getprops host)
	go (Spin host) = spin host
	go (Boot host) = boot (getprops host)
	go (Set host field val) = setPrivData host field val

spin :: HostName -> IO ()
spin host = do
	url <- getUrl
	privdata <- gpgDecrypt (privDataFile host)
	void $ boolSystem "git" [Param "commit", Param "-a", Param "-m", Param "propellor spin"]
	withHandle StdinHandle createProcessSuccess
		(proc "ssh" ["root@"++host, bootstrap url]) $ \h -> do
			hPutStr h $ unlines $ map (privDataMarker ++) $ lines privdata
			hClose h
  where
	bootstrap url = shellWrap $ intercalate " && "
		[ intercalate " ; "
			[ "if [ ! -d " ++ localdir ++ " ]"
			, "then"
			, intercalate " && "
				[ "apt-get -y install git"
				, "git clone " ++ url ++ " " ++ localdir
				]
			, "fi"
			]
		, "cd " ++ localdir
		, "make pull build"
		, "./propellor --boot " ++ host
		]

boot :: [Property] -> IO ()
boot props = do
	privdata <- map (drop $ length privDataMarker ) 
		. filter (privDataMarker `isPrefixOf`) 
		. lines 
		<$> getContents
	writeFileProtected privDataLocal (unlines privdata)
	ensureProperties props

localdir :: FilePath
localdir = "/usr/local/propellor"

getUrl :: IO String
getUrl = fromMaybe nourl <$> getM get urls
  where
	urls = ["remote.deploy.url", "remote.origin.url"]
	nourl = error $ "Cannot find deploy url in " ++ show urls
	get u = do
		v <- catchMaybeIO $ 
			takeWhile (/= '\n') 
				<$> readProcess "git" ["config", u]
		return $ case v of
			Just url | not (null url) -> Just url
			_ -> Nothing

module Property.Apt where

import Data.Maybe
import Control.Applicative
import Data.List
import System.IO
import Control.Monad

import Property
import Utility.SafeCommand
import Utility.Process

sourcesList :: FilePath
sourcesList = "/etc/apt/sources.list"

type Url = String
type Section = String

data Suite = Stable | Testing | Unstable | Experimental

showSuite :: Suite -> String
showSuite Stable = "stable"
showSuite Testing = "testing"
showSuite Unstable = "unstable"
showSuite Experimental = "experimental"

debLine :: Suite -> Url -> [Section] -> Line
debLine suite mirror sections = unwords $
	["deb", mirror, showSuite suite] ++ sections

srcLine :: Line -> Line
srcLine l = case words l of
	("deb":rest) -> unwords $ "deb-src" : rest
	_ -> ""

stdSections :: [Section]
stdSections = ["main", "contrib", "non-free"]

debCdn :: Suite -> [Line]
debCdn suite = [l, srcLine l]
  where
	l = debLine suite "http://cdn.debian.net/debian" stdSections

{- Makes sources.list have a standard content using the mirror CDN,
 - with a particular Suite. -}
stdSourcesList :: Suite -> Property
stdSourcesList = setSourcesList . debCdn

setSourcesList :: [Line] -> Property
setSourcesList ls = fileHasContent sourcesList ls `onChange` update

runApt :: [CommandParam] -> Property
runApt ps = cmdProperty' "apt-get" ps env
  where
	env =
		[ ("DEBIAN_FRONTEND", "noninteractive")
		, ("APT_LISTCHANGES_FRONTEND", "none")
		]

update :: Property
update = runApt [Param "update"]

upgrade :: Property
upgrade = runApt [Params "-y dist-upgrade"]

type Package = String

installed :: [Package] -> Property
installed ps = check (isInstallable ps) go
  where
	go = runApt $ [Param "-y", Param "install"] ++ map Param ps

removed :: [Package] -> Property
removed ps = check (or <$> isInstalled ps) go
  where
	go = runApt $ [Param "-y", Param "remove"] ++ map Param ps

isInstallable :: [Package] -> IO Bool
isInstallable ps = do
	l <- isInstalled ps
	return $ any (== False) l && not (null l)

{- Note that the order of the returned list will not always
 - correspond to the order of the input list. The number of items may
 - even vary. If apt does not know about a package at all, it will not
 - be included in the result list. -}
isInstalled :: [Package] -> IO [Bool]
isInstalled ps = catMaybes . map parse . lines
	<$> readProcess "apt-cache" ("policy":ps)
  where
	parse l
		| "Installed: (none)" `isInfixOf` l = Just False
		| "Installed: " `isInfixOf` l = Just True
		| otherwise = Nothing

autoRemove :: Property
autoRemove = runApt [Param "-y", Param "autoremove"]

unattendedUpgrades :: Bool -> Property
unattendedUpgrades enabled = installed ["unattended-upgrades"]
	`onChange` reConfigure "unattended-upgrades"
		[("unattended-upgrades/enable_auto_updates"
		 , "boolean"
		 , if enabled then "true" else "false")]

{- Preseeds debconf values and reconfigures the package so it takes
 - effect. -}
reConfigure :: Package -> [(String, String, String)] -> Property
reConfigure package vals = reconfigure `requires` setselections
  where
	setselections = IOProperty "preseed" $ makeChange $
		withHandle StdinHandle createProcessSuccess
			(proc "debconf-set-selections" []) $ \h -> do
				forM_ vals $ \(template, tmpltype, value) ->
					hPutStrLn h $ unwords [package, template, tmpltype, value]
				hClose h
	reconfigure = cmdProperty "dpkg-reconfigure" [Param "-fnone", Param package]

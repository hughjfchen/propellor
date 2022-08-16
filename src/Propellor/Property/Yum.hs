-- | Maintainer: Hugh JF Chen <hugh.jf.chen@gmail.com>
--
-- Support for the Yum package manager <https://www.centos.org/>
module Propellor.Property.Yum where

import Propellor.Base

runYum :: [String] -> UncheckedProperty CentOS
runYum ps = tightenTargets $ cmdProperty "yum" ps

-- | Have yum update its repo info, but without upgrading anything.
updateInfo :: Property CentOS
updateInfo =
  combineProperties "yum updateinfo" $
    props
      & runYum ["updateinfo"]
        `assume` MadeChange

-- | Have yum update to the same main version only.
update :: Property CentOS
update =
  combineProperties "yum update" $
    props
      & runYum ["-y", "update"]
        `assume` MadeChange

upgrade :: Property CentOS
upgrade =
  combineProperties "yum upgrade" $
    props
      & runYum ["-y", "upgrade"]
        `assume` MadeChange

type Package = String

installed :: [Package] -> Property CentOS
installed = installed' ["-y"]

installed' :: [String] -> [Package] -> Property CentOS
installed' params ps =
  check (not <$> isInstalled' ps) go
    `describe` unwords ("yum installed" : ps)
  where
    go = runYum (params ++ ["install"] ++ ps)

removed :: [Package] -> Property CentOS
removed ps =
  check
    (any (== IsInstalled) <$> getInstallStatus ps)
    (runYum (["-y", "erase"] ++ ps))
    `describe` unwords ("yum removed" : ps)

isInstalled :: Package -> IO Bool
isInstalled p = isInstalled' [p]

isInstalled' :: [Package] -> IO Bool
isInstalled' ps = all (== IsInstalled) <$> getInstallStatus ps

data InstallStatus = IsInstalled | NotInstalled
  deriving (Show, Eq)

{- Returns the InstallStatus of packages that are installed
 - or known and not installed. If a package is not known at all to apt
 - or dpkg, it is not included in the list. -}
getInstallStatus :: [Package] -> IO [InstallStatus]
getInstallStatus ps = mapMaybe id <$> mapM status ps
  where
    status :: Package -> IO (Maybe InstallStatus)
    status p = do
      ifM
        (succeeds "yum" ["-C", "list", "installed", p])
        ( return (Just IsInstalled),
          ifM
            (succeeds "yum" ["-C", "list", p])
            ( return (Just NotInstalled),
              return Nothing
            )
        )

succeeds :: String -> [String] -> IO Bool
succeeds cmd args =
  (quietProcess >> return True)
    `catchIO` (\_ -> return False)
  where
    quietProcess :: IO ()
    quietProcess = withQuietOutput createProcessSuccess p
    p = proc cmd args

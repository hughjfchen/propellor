-- | Maintainer: Félix Sipma <felix+propellor@gueux.org>

module Propellor.Property.Attic where

import Propellor.Base
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.Cron as Cron
import Data.List (intercalate)

installed :: Property DebianLike
installed = Apt.installed ["attic"]

repoExists :: FilePath -> IO Bool
repoExists repo = boolSystem "attic" [Param "list", File repo]

init :: FilePath -> Property DebianLike
init backupdir = check (not <$> repoExists backupdir) (cmdProperty "attic" initargs)
	`requires` installed
  where
	initargs =
		[ "init"
		, backupdir
		]

restored :: [FilePath] -> FilePath -> Property DebianLike
restored dirs backupdir = cmdProperty "attic" restoreargs
	`assume` MadeChange
	`describe` ("attic restore from " ++ backupdir)
	`requires` installed
  where
	restoreargs =
		[ "extract"
		, backupdir
		]
		++ dirs

backup :: [FilePath] -> FilePath -> Cron.Times -> [String] -> Property DebianLike
backup dirs backupdir crontimes extraargs = propertyList (backupdir ++ " attic backup") $ props
	& check (not <$> repoExists backupdir) (restored dirs backupdir)
	& Cron.niceJob ("attic_backup" ++ backupdir) crontimes (User "root") "/" backupcmd
	`requires` installed
  where
	backupcmd = intercalate ";"
		[ createCommand
		, pruneCommand
		]
	createCommand = unwords $
		[ "attic"
		, "create"
		, "--stats"
		]
		++ extraargs ++
		[ backupdir ++ "::" ++ "$(date --iso-8601=ns --utc)"
		, unwords dirs
		]
	pruneCommand = unwords
		[ "attic"
		, "prune"
		, backupdir
		, "--keep-daily=7"
		, "--keep-weekly=4"
		, "--keep-monthly=6"
		, "--keep-yearly=1"
		]

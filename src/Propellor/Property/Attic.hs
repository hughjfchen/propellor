-- | Maintainer: Félix Sipma <felix+propellor@gueux.org>

module Propellor.Property.Attic where

import Propellor.Base
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.Cron as Cron
import Data.List (intercalate)

installed :: Property NoInfo
installed = Apt.installed ["attic"]

repoExists :: FilePath -> IO Bool
repoExists repo = boolSystem "attic" [Param "list", File repo]

backup :: [FilePath] -> FilePath -> Cron.Times -> [String] -> Property NoInfo
backup dirs backupdir crontimes extraargs = propertyList (backupdir ++ " attic backup")
	[ installed
	, check (not <$> repoExists backupdir) $ cmdProperty "attic" initargs
	, Cron.niceJob ("attic_backup" ++ backupdir) crontimes (User "root") "/" backupcmd
	]
  where
	initargs =
		[ "init"
		, backupdir
		]
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

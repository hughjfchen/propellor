-- | Maintainer: Félix Sipma <felix+propellor@gueux.org>

module Propellor.Property.Attic
	( installed
	, repoExists
	, init
	, restored
	, backup
	, KeepPolicy (..)
	) where

import Propellor.Base hiding (init)
import Prelude hiding (init)
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.Cron as Cron
import Data.List (intercalate)

type AtticParam = String

type AtticRepo = FilePath

installed :: Property DebianLike
installed = Apt.installed ["attic"]

repoExists :: AtticRepo -> IO Bool
repoExists repo = boolSystem "attic" [Param "list", File repo]

init :: AtticRepo -> Property DebianLike
init backupdir = check (not <$> repoExists backupdir) (cmdProperty "attic" initargs)
	`requires` installed
  where
	initargs =
		[ "init"
		, backupdir
		]

restored :: [FilePath] -> AtticRepo -> Property DebianLike
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

backup :: [FilePath] -> AtticRepo -> Cron.Times -> [AtticParam] -> [KeepPolicy] -> Property DebianLike
backup dirs backupdir crontimes extraargs kp = propertyList (backupdir ++ " attic backup") $ props
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
	pruneCommand = unwords $
		[ "attic"
		, "prune"
		, backupdir
		]
		++
		map keepParam kp

keepParam :: KeepPolicy -> AtticParam
keepParam (KeepHours n) = "--keep-hourly=" ++ show n
keepParam (KeepDays n) = "--keep-daily=" ++ show n
keepParam (KeepWeeks n) = "--keep-daily=" ++ show n
keepParam (KeepMonths n) = "--keep-monthly=" ++ show n
keepParam (KeepYears n) = "--keep-yearly=" ++ show n

data KeepPolicy
	= KeepHours Int
	| KeepDays Int
	| KeepWeeks Int
	| KeepMonths Int
	| KeepYears Int

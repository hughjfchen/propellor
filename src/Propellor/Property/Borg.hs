-- | Maintainer: Félix Sipma <felix+propellor@gueux.org>
--
-- Support for the Borg backup tool <https://github.com/borgbackup>

module Propellor.Property.Borg
	( BorgParam
	, BorgRepo(..)
	, BorgRepoOpt(..)
	, installed
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

-- | Parameter to pass to a borg command.
type BorgParam = String

-- | A borg repository.
data BorgRepo
	-- | Location of the repository, eg
	-- `BorgRepo "root@myserver:/mnt/backup/git.borg"`
	= BorgRepo String
	-- | Location of the repository, and additional options to use
	-- when accessing the repository.
	| BorgRepoUsing [BorgRepoOpt] String

data BorgRepoOpt 
	-- | Use to specify a ssh private key to use when accessing a
	-- BorgRepo.
	= UseSshKey FilePath
	-- | Use to specify an environment variable to set when running
	-- borg on a BorgRepo.
	| UsesEnvVar (String, String)

repoLoc :: BorgRepo -> String
repoLoc (BorgRepo s) = s
repoLoc (BorgRepoUsing _ s) = s

runBorg :: BorgRepo -> [CommandParam] -> IO Bool
runBorg repo ps = case runBorgEnv repo of
	[] -> boolSystem "borg" ps
	environ -> do
		environ' <- addEntries environ <$> getEnvironment
		boolSystemEnv "borg" ps (Just environ')

runBorgEnv :: BorgRepo -> [(String, String)]
runBorgEnv (BorgRepo _) = []
runBorgEnv (BorgRepoUsing os _) = map go os
  where
	go (UseSshKey k) = ("BORG_RSH", "ssh -i " ++ k)
	go (UsesEnvVar (k, v)) = (k, v)

installed :: Property DebianLike
installed = pickOS installdebian aptinstall
  where
	installdebian :: Property Debian
	installdebian = withOS desc $ \w o -> case o of
		(Just (System (Debian _ (Stable "jessie")) _)) -> ensureProperty w $
			Apt.backportInstalled ["borgbackup", "python3-msgpack"]
		_ -> ensureProperty w $
			Apt.installed ["borgbackup"]
	aptinstall = Apt.installed ["borgbackup"] `describe` desc
        desc = "installed borgbackup"

repoExists :: BorgRepo -> IO Bool
repoExists repo = runBorg repo [Param "list", Param (repoLoc repo)]

-- | Inits a new borg repository
init :: BorgRepo -> Property DebianLike
init repo = check (not <$> repoExists repo)
	(cmdPropertyEnv "borg" initargs (runBorgEnv repo))
		`requires` installed
  where
	initargs =
		[ "init"
		, repoLoc repo
		]

-- | Restores a directory from a borg backup.
--
-- Only does anything if the directory does not exist, or exists,
-- but is completely empty.
--
-- The restore is performed atomically; restoring to a temp directory
-- and then moving it to the directory.
restored :: FilePath -> BorgRepo -> Property DebianLike
restored dir repo = go `requires` installed
  where
	go :: Property DebianLike
	go = property (dir ++ " restored by borg") $ ifM (liftIO needsRestore)
		( do
			warningMessage $ dir ++ " is empty/missing; restoring from backup ..."
			liftIO restore
		, noChange
		)

	needsRestore = isUnpopulated dir

	restore = withTmpDirIn (takeDirectory dir) "borg-restore" $ \tmpdir -> do
		ok <- runBorg repo $
			[ Param "extract"
			, Param (repoLoc repo)
			, Param tmpdir
			]
		let restoreddir = tmpdir ++ "/" ++ dir
		ifM (pure ok <&&> doesDirectoryExist restoreddir)
			( do
				void $ tryIO $ removeDirectory dir
				renameDirectory restoreddir dir
				return MadeChange
			, return FailedChange
			)

-- | Installs a cron job that causes a given directory to be backed
-- up, by running borg with some parameters.
--
-- If the directory does not exist, or exists but is completely empty,
-- this Property will immediately restore it from an existing backup.
--
-- So, this property can be used to deploy a directory of content
-- to a host, while also ensuring any changes made to it get backed up.
-- For example:
--
-- >	& Borg.backup "/srv/git"
-- >		(BorgRepo "root@myserver:/mnt/backup/git.borg") 
-- >		Cron.Daily
-- >		["--exclude=/srv/git/tobeignored"]
-- >		[Borg.KeepDays 7, Borg.KeepWeeks 4, Borg.KeepMonths 6, Borg.KeepYears 1]
--
-- Note that this property does not initialize the backup repository,
-- so that will need to be done once, before-hand.
--
-- Since borg uses a fair amount of system resources, only one borg
-- backup job will be run at a time. Other jobs will wait their turns to
-- run.
backup :: FilePath -> BorgRepo -> Cron.Times -> [BorgParam] -> [KeepPolicy] -> Property DebianLike
backup dir repo crontimes extraargs kp = backup' dir repo crontimes extraargs kp
	`requires` restored dir repo

-- | Does a backup, but does not automatically restore.
backup' :: FilePath -> BorgRepo -> Cron.Times -> [BorgParam] -> [KeepPolicy] -> Property DebianLike
backup' dir repo crontimes extraargs kp = cronjob
	`describe` desc
	`requires` installed
  where
	desc = repoLoc repo ++ " borg backup"
	cronjob = Cron.niceJob ("borg_backup" ++ dir) crontimes (User "root") "/" $
		"flock " ++ shellEscape lockfile ++ " sh -c " ++ shellEscape backupcmd
	lockfile = "/var/lock/propellor-borg.lock"
	backupcmd = intercalate "&&" $ concat
		[ concatMap exportenv (runBorgEnv repo)
		, [createCommand]
		, if null kp then [] else [pruneCommand]
		]
	exportenv (k, v) = 
		[ k ++ "=" ++ shellEscape v
		, "export " ++ k
		]
	createCommand = unwords $
		[ "borg"
		, "create"
		, "--stats"
		]
		++ map shellEscape extraargs ++
		[ shellEscape (repoLoc repo) ++ "::" ++ "$(date --iso-8601=ns --utc)"
		, shellEscape dir
		]
	pruneCommand = unwords $
		[ "borg"
		, "prune"
		, shellEscape (repoLoc repo)
		]
		++
		map keepParam kp

-- | Constructs an BorgParam that specifies which old backup generations to
-- keep. By default, all generations are kept. However, when this parameter is
-- passed to the `backup` property, it will run borg prune to clean out
-- generations not specified here.
keepParam :: KeepPolicy -> BorgParam
keepParam (KeepHours n) = "--keep-hourly=" ++ val n
keepParam (KeepDays n) = "--keep-daily=" ++ val n
keepParam (KeepWeeks n) = "--keep-daily=" ++ val n
keepParam (KeepMonths n) = "--keep-monthly=" ++ val n
keepParam (KeepYears n) = "--keep-yearly=" ++ val n

-- | Policy for backup generations to keep. For example, KeepDays 30 will
-- keep the latest backup for each day when a backup was made, and keep the
-- last 30 such backups. When multiple KeepPolicies are combined together,
-- backups meeting any policy are kept. See borg's man page for details.
data KeepPolicy
	= KeepHours Int
	| KeepDays Int
	| KeepWeeks Int
	| KeepMonths Int
	| KeepYears Int

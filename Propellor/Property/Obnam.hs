module Propellor.Property.Obnam where

import Propellor
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.Cron as Cron
import Utility.SafeCommand

import Data.List

installed :: Property
installed = Apt.installed ["obnam"]

type ObnamParam = String

-- | An obnam repository can be used by multiple clients. Obnam uses
-- locking to allow only one client to write at a time. Since stale lock
-- files can prevent backups from happening, it's more robust, if you know
-- a repository has only one client, to force the lock before starting a
-- backup. Using OnlyClient allows propellor to do so when running obnam.
data NumClients = OnlyClient | MultipleClients
	deriving (Eq)

-- | Installs a cron job that causes a given directory to be backed
-- up, by running obnam with some parameters.
--
-- If the directory does not exist, or exists but is completely empty,
-- this Property will immediately restore it from an existing backup.
--
-- So, this property can be used to deploy a directory of content
-- to a host, while also ensuring any changes made to it get backed up.
-- And since Obnam encrypts, just make this property depend on a gpg
-- key, and tell obnam to use the key, and your data will be backed
-- up securely. For example: 
--
-- >	& Obnam.backup "/srv/git" "33 3 * * *"
-- >		[ "--repository=sftp://2318@usw-s002.rsync.net/~/mygitrepos.obnam"
-- >		, "--encrypt-with=1B169BE1"
-- >		] Obnam.OnlyClient
-- >		`requires` Gpg.keyImported "1B169BE1" "root"
-- >		`requires` Ssh.keyImported SshRsa "root"
--
-- How awesome is that?
backup :: FilePath -> Cron.CronTimes -> [ObnamParam] -> NumClients -> Property
backup dir crontimes params numclients = cronjob `describe` desc
	`requires` restored dir params
  where
	desc = dir ++ " backed up by obnam"
	cronjob = Cron.niceJob ("obnam_backup" ++ dir) crontimes "root" "/" $
		intercalate ";" $ catMaybes
			[ if numclients == OnlyClient
				then Just $ unwords $
					[ "obnam"
					, "force-lock"
					] ++ map shellEscape params
				else Nothing
			, Just $ unwords $
				[ "obnam"
				, "backup"
				, shellEscape dir
				] ++ map shellEscape params
			]

-- | Restores a directory from an obnam backup.
--
-- Only does anything if the directory does not exist, or exists,
-- but is completely empty.
--
-- The restore is performed atomically; restoring to a temp directory
-- and then moving it to the directory.
restored :: FilePath -> [ObnamParam] -> Property
restored dir params = Property (dir ++ " restored by obnam") go
	`requires` installed
  where
	go = ifM (liftIO needsRestore)
		( liftIO restore
		, noChange
		)

	needsRestore = null <$> catchDefaultIO [] (dirContents dir)

	restore = withTmpDirIn (takeDirectory dir) "obnam-restore" $ \tmpdir -> do
		ok <- boolSystem "obnam" $
			[ Param "restore"
			, Param "--to"
			, Param tmpdir
			] ++ map Param params
		let restoreddir = tmpdir ++ "/" ++ dir
		ifM (pure ok <&&> doesDirectoryExist restoreddir)
			( do
				void $ tryIO $ removeDirectory dir
				renameDirectory restoreddir dir
				return MadeChange
			, return FailedChange
			)

module Propellor.Property.Obnam where

import Propellor
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.Cron as Cron
import qualified Propellor.Property.Gpg as Gpg

import Data.List

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
-- >		] Obnam.OnlyClient
-- >		`requires` Ssh.keyImported SshRsa "root" (Context hostname)
--
-- How awesome is that?
backup :: FilePath -> Cron.Times -> [ObnamParam] -> NumClients -> Property NoInfo
backup dir crontimes params numclients =
	backup' dir crontimes params numclients
		`requires` restored dir params

-- | Like backup, but the specified gpg key id is used to encrypt
-- the repository. 
--
-- The gpg secret key will be automatically imported
-- into root's keyring using Propellor.Property.Gpg.keyImported
backupEncrypted :: FilePath -> Cron.Times -> [ObnamParam] -> NumClients -> Gpg.GpgKeyId -> Property HasInfo
backupEncrypted dir crontimes params numclients keyid =
	backup dir crontimes params' numclients
		`requires` Gpg.keyImported keyid (User "root")
  where
	params' = ("--encrypt-with=" ++ Gpg.getGpgKeyId keyid) : params

-- | Does a backup, but does not automatically restore.
backup' :: FilePath -> Cron.Times -> [ObnamParam] -> NumClients -> Property NoInfo
backup' dir crontimes params numclients = cronjob `describe` desc
  where
	desc = dir ++ " backed up by obnam"
	cronjob = Cron.niceJob ("obnam_backup" ++ dir) crontimes (User "root") "/" $
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
restored :: FilePath -> [ObnamParam] -> Property NoInfo
restored dir params = property (dir ++ " restored by obnam") go
	`requires` installed
  where
	go = ifM (liftIO needsRestore)
		( do
			warningMessage $ dir ++ " is empty/missing; restoring from backup ..."
			liftIO restore
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

installed :: Property NoInfo
installed = Apt.installed ["obnam"]

module Propellor.Property.Obnam where

import Propellor
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.Cron as Cron
import Utility.SafeCommand

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

installed :: Property
installed = Apt.installed ["obnam"]

-- | Ensures that a recent version of obnam gets installed.
--
-- Only does anything for Debian Stable.
latestVersion :: Property
latestVersion = withOS "obnam latest version" $ \o -> case o of
	(Just (System (Debian suite) _)) | isStable suite -> ensureProperty $
		Apt.setSourcesListD (sources suite) "obnam"
			`requires` toProp (Apt.trustsKey key)
	_ -> noChange
  where
	sources suite = 
		[ "deb http://code.liw.fi/debian " ++ Apt.showSuite suite ++ " main"
		]
	-- gpg key used by the code.liw.fi repository.
	key = Apt.AptKey "obnam" $ unlines
		[ "-----BEGIN PGP PUBLIC KEY BLOCK-----"
		, "Version: GnuPG v1.4.9 (GNU/Linux)"
		, ""
		, "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"
		, "=i2c3"
		, "-----END PGP PUBLIC KEY BLOCK-----"
		]

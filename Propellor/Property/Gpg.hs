module Propellor.Property.Gpg where

import Propellor
import qualified Propellor.Property.Apt as Apt
import Utility.FileSystemEncoding

import System.PosixCompat

installed :: Property
installed = Apt.installed ["gnupg"]

-- | Sets up a user with a gpg key from the privdata.
--
-- Note that if a secret key is exported using gpg -a --export-secret-key,
-- the public key is also included. Or just a public key could be
-- exported, and this would set it up just as well.
--
-- Recommend only using this for low-value dedicated role keys.
-- No attempt has been made to scrub the key out of memory once it's used.
--
-- The GpgKeyId does not have to be a numeric id; it can just as easily
-- be a description of the key.
keyImported :: GpgKeyId -> UserName -> Property
keyImported keyid user = flagFile' (property desc go) genflag
	`requires` installed
  where
	desc = user ++ " has gpg key " ++ show keyid
	genflag = do
		d <- dotDir user
		return $ d </> ".propellor-imported-keyid-" ++ keyid
	go = withPrivData (GpgKey keyid) $ \key -> makeChange $
		withHandle StdinHandle createProcessSuccess
			(proc "su" ["-c", "gpg --import", user]) $ \h -> do
				fileEncoding h
				hPutStr h key
				hClose h

dotDir :: UserName -> IO FilePath
dotDir user = do
	home <- homeDirectory <$> getUserEntryForName user
	return $ home </> ".gnupg"

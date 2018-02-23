-- | Maintainer: Félix Sipma <felix+propellor@gueux.org>

module Propellor.Property.Dhparams where

import Propellor.Base
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.File as File
import Utility.FileMode
import Utility.SafeCommand


length' :: Int
length' = 2048

file :: FilePath
file = "/etc/ssl/private/dhparams.pem"

safeDhparams :: Property DebianLike
safeDhparams = propertyList "safe dhparams" $ props
	& File.dirExists (takeDirectory file)
	& Apt.installed ["openssl"]
	& check (not <$> doesFileExist file) (createDhparams file length')

createDhparams :: FilePath -> Int -> Property UnixLike
createDhparams f l = property ("generate new dhparams: " ++ f) $ liftIO $ withUmask 0o0177 $ withFile f WriteMode $ \h ->
	cmdResult <$> boolSystem' "openssl" [Param "dhparam", Param (show l)] (\p -> p { std_out = UseHandle h })

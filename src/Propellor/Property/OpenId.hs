module Propellor.Property.OpenId where

import Propellor
import qualified Propellor.Property.File as File
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.Service as Service

import Data.List

providerFor :: [User] -> String -> Property HasInfo
providerFor users baseurl = propertyList desc $ map toProp
	[ Apt.serviceInstalledRunning "apache2"
	, Apt.installed ["simpleid"]
		`onChange` Service.restarted "apache2"
	, File.fileProperty (desc ++ " configured")
		(map setbaseurl) "/etc/simpleid/config.inc"
	] ++ map identfile users
  where
	url = "http://"++baseurl++"/simpleid"
	desc = "openid provider " ++ url
	setbaseurl l
		| "SIMPLEID_BASE_URL" `isInfixOf` l = 
			"define('SIMPLEID_BASE_URL', '"++url++"');"
		| otherwise = l
	
	-- the identities directory controls access, so open up
	-- file mode
	identfile (User u) = File.hasPrivContentExposed
		(concat [ "/var/lib/simpleid/identities/", u, ".identity" ])
		(Context baseurl)

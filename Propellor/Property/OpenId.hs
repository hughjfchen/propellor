module Propellor.Property.OpenId where

import Propellor
import qualified Propellor.Property.File as File
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.Service as Service
import Utility.FileMode

import Data.List
import System.Posix.Files

providerFor :: [UserName] -> String -> Property
providerFor users baseurl = propertyList desc $
	[ Apt.serviceInstalledRunning "apache2"
	, Apt.installed ["simpleid"]
		`onChange` Service.restarted "apache2"
	, File.fileProperty desc
		(map setbaseurl) "/etc/simpleid/config.inc"
	] ++ map identfile users
  where
	url = "http://"++baseurl++"/simpleid"
	desc = "openid provider " ++ url
	setbaseurl l
		| "SIMPLEID_BASE_URL" `isInfixOf` l = 
			"define('SIMPLEID_BASE_URL', '"++url++"');"
		| otherwise = l
	
	identfile u = combineProperties desc
		[ File.hasPrivContent f
		-- the identitites directory controls access, so open up
		-- file mode
		, File.mode f (combineModes (ownerWriteMode:readModes))
		]
	  where
		f = concat $ [ "/var/lib/simpleid/identities/", u, ".identity" ]

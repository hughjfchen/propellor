module Propellor.Property.Hostname where

import Propellor
import qualified Propellor.Property.File as File

-- | Ensures that the hostname is set to the HostInfo value.
-- Configures /etc/hostname and the current hostname.
--
-- A FQDN also configures /etc/hosts, with an entry for 127.0.1.1, which is
-- standard at least on Debian to set the FDQN (127.0.0.1 is localhost).
sane :: Property
sane = property ("sane hostname") (ensureProperty . setTo =<< asks hostName)

setTo :: HostName -> Property
setTo hn = combineProperties desc go
  where
	desc = "hostname " ++ hn
	(basehost, domain) = separate (== '.') hn

	go = catMaybes
		[ Just $ "/etc/hostname" `File.hasContent` [basehost]
		, if null domain
			then Nothing 
			else Just $ File.fileProperty desc
				addhostline "/etc/hosts"
		, Just $ trivial $ cmdProperty "hostname" [basehost]
		]
	
	hostip = "127.0.1.1"
	hostline = hostip ++ "\t" ++ hn ++ " " ++ basehost

	addhostline ls = hostline : filter (not . hashostip) ls
	hashostip l = headMaybe (words l) == Just hostip

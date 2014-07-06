module Propellor.Property.Hostname where

import Propellor
import qualified Propellor.Property.File as File

-- | Ensures that the hostname is set using best practices.
--
-- Configures /etc/hostname and the current hostname.
--
-- /etc/hosts is also configured, with an entry for 127.0.1.1, which is
-- standard at least on Debian to set the FDQN.
--
-- Also, the /etc/hosts 127.0.0.1 line is set to localhost. Putting any
-- other hostnames there is not best practices and can lead to annoying
-- messages from eg, apache.
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
			else Just $ hostsline "127.0.1.1" [hn, basehost]
		, Just $ hostsline "127.0.0.1" ["localhost"]
		, Just $ trivial $ cmdProperty "hostname" [basehost]
		]
	
	hostsline ip names = File.fileProperty desc
		(addhostsline ip names)
		"/etc/hosts"
	addhostsline ip names ls =
		(ip ++ "\t" ++ (unwords names)) : filter (not . hasip ip) ls
	hasip ip l = headMaybe (words l) == Just ip

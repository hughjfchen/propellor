module Propellor.Property.Hostname where

import Propellor.Base
import qualified Propellor.Property.File as File

import Data.List

-- | Ensures that the hostname is set using best practices.
--
-- Configures </etc/hostname> and the current hostname.
--
-- Configures </etc/mailname> with the domain part of the hostname.
--
-- </etc/hosts> is also configured, with an entry for 127.0.1.1, which is
-- standard at least on Debian to set the FDQN.
--
-- Also, the </etc/hosts> 127.0.0.1 line is set to localhost. Putting any
-- other hostnames there is not best practices and can lead to annoying
-- messages from eg, apache.
sane :: Property NoInfo
sane = property ("sane hostname") (ensureProperty . setTo =<< asks hostName)

setTo :: HostName -> Property NoInfo
setTo hn = combineProperties desc go
  where
	desc = "hostname " ++ hn
	(basehost, domain) = separate (== '.') hn

	go = catMaybes
		[ Just $ "/etc/hostname" `File.hasContent` [basehost]
		, if null domain
			then Nothing 
			else Just $ trivial $ hostsline "127.0.1.1" [hn, basehost]
		, Just $ trivial $ hostsline "127.0.0.1" ["localhost"]
		, Just $ trivial $ cmdProperty "hostname" [basehost]
		, Just $ "/etc/mailname" `File.hasContent`
			[if null domain then hn else domain]
		]
	
	hostsline ip names = File.fileProperty desc
		(addhostsline ip names)
		"/etc/hosts"
	addhostsline ip names ls =
		(ip ++ "\t" ++ (unwords names)) : filter (not . hasip ip) ls
	hasip ip l = headMaybe (words l) == Just ip

-- | Makes </etc/resolv.conf> contain search and domain lines for 
-- the domain that the hostname is in.
searchDomain :: Property NoInfo
searchDomain = property desc (ensureProperty . go =<< asks hostName)
  where
	desc = "resolv.conf search and domain configured"
	go hn =
		let (_basehost, domain) = separate (== '.') hn
		in  File.fileProperty desc (use domain) "/etc/resolv.conf"
	use domain ls = filter wanted $ nub (ls ++ cfgs)
	  where
		cfgs = ["domain " ++ domain, "search " ++ domain]
		wanted l
			| l `elem` cfgs = True
			| "domain " `isPrefixOf` l = False
			| "search " `isPrefixOf` l = False
			| otherwise = True

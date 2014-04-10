module Propellor.Property.Git where

import Propellor
import Propellor.Property.File
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.Service as Service

import Data.List

-- | Exports all git repos in a directory (that user nobody can read)
-- using git-daemon, run from inetd.
--
-- Note that reverting this property does not remove or stop inetd.
daemonRunning :: FilePath -> RevertableProperty
daemonRunning exportdir = RevertableProperty setup unsetup
  where
	setup = containsLine conf (mkl "tcp4")
		`requires`
		containsLine conf (mkl "tcp6")
		`requires`
		dirExists exportdir
		`requires`
		Apt.serviceInstalledRunning "openbsd-inetd"
		`onChange`
		Service.reloaded "openbsd-inetd"
	unsetup = lacksLine conf (mkl "tcp4")
		`requires`
		lacksLine conf (mkl "tcp6")
		`onChange`
		Service.reloaded "openbsd-inetd"

	conf = "/etc/inetd.conf"

	mkl tcpv = intercalate "\t"
		[ "git"
		, "stream"
		, tcpv
		, "nowait"
		, "nobody"
		, "/usr/bin/git"
		, "git"
		, "daemon"
		, "--inetd"
		, "--export-all"
		, "--base-path=" ++ exportdir
		, exportdir
		]

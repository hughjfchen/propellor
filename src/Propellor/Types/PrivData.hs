module Propellor.Types.PrivData where

import Propellor.Types.OS

-- | Note that removing or changing field names will break the
-- serialized privdata files, so don't do that!
-- It's fine to add new fields.
data PrivDataField
	= DockerAuthentication
	| SshPubKey SshKeyType UserName
	| SshPrivKey SshKeyType UserName
	| SshAuthorizedKeys UserName
	| Password UserName
	| PrivFile FilePath
	| GpgKey
	deriving (Read, Show, Ord, Eq)

-- | Context in which a PrivDataField is used.
--
-- Often this will be a domain name. For example, 
-- Context "www.example.com" could be used for the SSL cert
-- for the web server serving that domain. Multiple hosts might
-- use that privdata.
newtype Context = Context String
	deriving (Read, Show, Ord, Eq)

-- | Use when a PrivDataField is not dependent on any paricular context.
anyContext :: Context
anyContext = Context "any"

type PrivData = String

data SshKeyType = SshRsa | SshDsa | SshEcdsa | SshEd25519
	deriving (Read, Show, Ord, Eq)

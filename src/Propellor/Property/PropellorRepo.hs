{-# LANGUAGE DeriveDataTypeable #-}

module Propellor.Property.PropellorRepo where

import Propellor.Base
import Propellor.Git.Config
import Propellor.Types.Info

-- | Sets the url to use as the origin of propellor's git repository.
--
-- By default, the url is taken from the deploy or origin remote of
-- the repository that propellor --spin is run in. Setting this property
-- overrides that default behavior with a different url.
--
-- When hosts are being updated without using -- --spin, eg when using
-- the `Propellor.Property.Cron.runPropellor` cron job, this property can
-- be set to redirect them to a new git repository url.
hasOriginUrl :: String -> Property (HasInfo + UnixLike)
hasOriginUrl u = setInfoProperty p (toInfo (InfoVal (OriginUrl u)))
  where
	p :: Property UnixLike
	p = property ("propellor repo url " ++ u) $ do
		curru <- liftIO getRepoUrl
		if curru == Just u
			then return NoChange
			else makeChange $ setRepoUrl u

newtype OriginUrl = OriginUrl String
	deriving (Show, Typeable)

{-# LANGUAGE PackageImports #-}

module Propellor.Engine where

import System.Exit
import System.IO
import Data.Monoid
import Control.Applicative
import System.Console.ANSI
import "mtl" Control.Monad.Reader
import Control.Exception (bracket)
import System.PosixCompat
import System.Posix.IO
import System.FilePath
import System.Directory

import Propellor.Types
import Propellor.Message
import Propellor.Exception
import Propellor.Info
import Utility.Exception
import Utility.PartialPrelude
import Utility.Monad

runPropellor :: Host -> Propellor a -> IO a
runPropellor host a = runReaderT (runWithHost a) host

mainProperties :: Host -> IO ()
mainProperties host = do
	r <- runPropellor host $
		ensureProperties [Property "overall" (ensureProperties $ hostProperties host) mempty]
	h <- mkMessageHandle
        whenConsole h $
		setTitle "propellor: done"
	hFlush stdout
	case r of
		FailedChange -> exitWith (ExitFailure 1)
		_ -> exitWith ExitSuccess

ensureProperties :: [Property] -> Propellor Result
ensureProperties ps = ensure ps NoChange
  where
	ensure [] rs = return rs
	ensure (l:ls) rs = do
		hn <- asks hostName
		r <- actionMessageOn hn (propertyDesc l) (ensureProperty l)
		ensure ls (r <> rs)

ensureProperty :: Property -> Propellor Result
ensureProperty = catchPropellor . propertySatisfy

-- | Lifts an action into a different host.
--
-- For example, `fromHost hosts "otherhost" getSshPubKey`
fromHost :: [Host] -> HostName -> Propellor a -> Propellor (Maybe a)
fromHost l hn getter = case findHost l hn of
	Nothing -> return Nothing
	Just h -> liftIO $ Just <$>
		runReaderT (runWithHost getter) h

onlyProcess :: FilePath -> IO a -> IO a
onlyProcess lockfile a = bracket lock unlock (const a)
  where
	lock = do
		createDirectoryIfMissing True (takeDirectory lockfile)
		l <- createFile lockfile stdFileMode
		setLock l (WriteLock, AbsoluteSeek, 0, 0)
			`catchIO` const alreadyrunning
		return l
	unlock = closeFd
	alreadyrunning = error "Propellor is already running on this host!"

-- | Reads and displays each line from the Handle, except for the last line
-- which is a Result.
processChainOutput :: Handle -> IO Result
processChainOutput h = go Nothing
  where
	go lastline = do
		v <- catchMaybeIO (hGetLine h)
		case v of
			Nothing -> case lastline of
				Nothing -> pure FailedChange
				Just l -> case readish l of
					Just r -> pure r
					Nothing -> do
						putStrLn l
						hFlush stdout
						return FailedChange
			Just s -> do
				maybe noop (\l -> unless (null l) (putStrLn l)) lastline
				hFlush stdout
				go (Just s)

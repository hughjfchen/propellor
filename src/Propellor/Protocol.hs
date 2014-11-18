-- | This is a simple line-based protocol used for communication between
-- a local and remote propellor. It's sent over a ssh channel, and lines of
-- the protocol can be interspersed with other, non-protocol lines
-- that should just be passed through to be displayed.

module Propellor.Protocol where

import Data.List

import Propellor

data Stage = Ready | NeedGitClone | NeedRepoUrl | NeedPrivData | NeedGitPush
	deriving (Read, Show, Eq)

type Marker = String
type Marked = String

statusMarker :: Marker
statusMarker = "STATUS"

privDataMarker :: String
privDataMarker = "PRIVDATA "

repoUrlMarker :: String
repoUrlMarker = "REPOURL "

gitPushMarker :: String
gitPushMarker = "GITPUSH"

toMarked :: Marker -> String -> String
toMarked marker = ++

fromMarked :: Marker -> Marked -> Maybe String
fromMarked marker s
	| marker `isPrefixOf` s = Just $ drop (length marker) s
	| otherwise = Nothing

sendMarked :: Handle -> Marker -> String -> IO ()
sendMarked h marker s = do
	-- Prefix string with newline because sometimes a
	-- incomplete line has been output, and the marker needs to
	-- come at the start of a line.
	hPutStrLn h ("\n" ++ toMarked marker s)
	hFlush h

getMarked :: Handle -> Marker -> IO (Maybe String)
getMarked h marker = go =<< catchMaybeIO (hGetLine h)
  where
	go Nothing = return Nothing
	go (Just l) = case fromMarked marker l of
		Nothing -> do
			putStrLn l
			getMarked h marker
		Just v -> return (Just v)

req :: Stage -> Marker -> (String -> IO ()) -> IO ()
req stage marker a = do
	sendMarked stdout statusMarker (show stage)
	maybe noop a =<< getMarked stdin marker

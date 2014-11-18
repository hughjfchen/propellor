-- | This is a simple line-based protocol used for communication between
-- a local and remote propellor. It's sent over a ssh channel, and lines of
-- the protocol can be interspersed with other, non-protocol lines
-- that should just be passed through to be displayed.

module Propellor.Protocol where

import Data.List

import Propellor

data BootStrapStatus = Ready | NeedGitClone
	deriving (Read, Show, Eq)

type Marker = String
type Marked = String

statusMarker :: Marker
statusMarker = "STATUS"

privDataMarker :: String
privDataMarker = "PRIVDATA "

toMarked :: Marker -> String -> String
toMarked marker = intercalate "\n" . map (marker ++) . lines

sendMarked :: Handle -> Marker -> String -> IO ()
sendMarked h marker s = do
	-- Prefix string with newline because sometimes a
	-- incomplete line is output.
	hPutStrLn h ("\n" ++ toMarked marker s)
	hFlush h

fromMarked :: Marker -> Marked -> Maybe String
fromMarked marker s
	| marker `isPrefixOf` s = Just $ drop (length marker) s
	| otherwise = Nothing

getMarked :: Handle -> Marker -> IO (Maybe String)
getMarked h marker = go =<< catchMaybeIO (hGetLine h)
  where
	go Nothing = return Nothing
	go (Just l) = case fromMarked marker l of
		Nothing -> do
			putStrLn l
			getMarked h marker
		Just v -> return (Just v)

-- | Simple server, using a named pipe. Client connects, sends a command,
-- and gets back all the output from the command, in a stream.
--
-- This is useful for eg, docker.

module Propellor.SimpleSh where

import Network.Socket
import Control.Concurrent.Chan
import Control.Concurrent.Async
import System.Process (std_in, std_out, std_err)

import Propellor
import Utility.FileMode
import Utility.ThreadScheduler

data Cmd = Cmd String [String]
	deriving (Read, Show)

data Resp = StdoutLine String | StderrLine String | Done
	deriving (Read, Show)

simpleSh :: FilePath -> IO ()
simpleSh namedpipe = do
	nukeFile namedpipe
	let dir = takeDirectory namedpipe
	createDirectoryIfMissing True dir
	modifyFileMode dir (removeModes otherGroupModes)
	s <- socket AF_UNIX Stream defaultProtocol
	bindSocket s (SockAddrUnix namedpipe)
	listen s 2
	forever $ do
		(client, _addr) <- accept s
		h <- socketToHandle client ReadWriteMode
		maybe noop (run h) . readish =<< hGetLine h
  where
	run h (Cmd cmd params) = do
		let p = (proc cmd params)
                	{ std_in = Inherit
	                , std_out = CreatePipe
			, std_err = CreatePipe
			}
		(Nothing, Just outh, Just errh, pid) <- createProcess p
		chan <- newChan

		let runwriter = do
			v <- readChan chan
			hPutStrLn h (show v)
			hFlush h
			case v of
				Done -> noop
				_ -> runwriter
		writer <- async runwriter

		let mkreader t from = maybe noop (const $ mkreader t from) 
			=<< catchMaybeIO (writeChan chan . t =<< hGetLine from)
		void $ concurrently
			(mkreader StdoutLine outh)
			(mkreader StderrLine errh)
		
		void $ tryIO $ waitForProcess pid

		writeChan chan Done

		wait writer

		hClose outh
		hClose errh
		hClose h

simpleShClient :: FilePath -> String -> [String] -> ([Resp] -> IO a) -> IO a
simpleShClient namedpipe cmd params handler = do
	debug ["simplesh connecting"]
	s <- socket AF_UNIX Stream defaultProtocol
	connect s (SockAddrUnix namedpipe)
	h <- socketToHandle s ReadWriteMode
	hPutStrLn h $ show $ Cmd cmd params
	hFlush h
	debug ["simplesh sent command"]
	resps <- catMaybes . map readish . lines <$> hGetContents h
	v <- hClose h `after` handler resps
	debug ["simplesh processed response"]
	return v

simpleShClientRetry :: Int -> FilePath -> String -> [String] -> ([Resp] -> IO a) -> IO a
simpleShClientRetry retries namedpipe cmd params handler = go retries
  where
	run = simpleShClient namedpipe cmd params handler
	go n
		| n < 1 = run
		| otherwise = do
			v <- tryIO run
			case v of
				Right r -> return r
				Left e -> do
					debug ["simplesh connection retry", show e]
					threadDelaySeconds (Seconds 1)
					go (n - 1)

getStdout :: Resp -> Maybe String
getStdout (StdoutLine s) = Just s
getStdout _ = Nothing

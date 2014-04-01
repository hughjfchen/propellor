-- | Simple server, using a named pipe. Client connects, sends a command,
-- and gets back all the output from the command, in a stream.
--
-- This is useful for eg, docker.

module Propellor.SimpleSh where

import Network.Socket
import Control.Concurrent.Chan
import Control.Concurrent.Async
import System.Process (std_in, std_out, std_err)
import System.Exit

import Propellor

data Cmd = Cmd String [String]
	deriving (Read, Show)

data Resp = StdoutLine String | StderrLine String | Done ExitCode
	deriving (Read, Show)

simpleSh :: FilePath -> IO ()
simpleSh namedpipe = do
	nukeFile namedpipe
	createDirectoryIfMissing True (takeDirectory namedpipe)
	s <- socket AF_UNIX Stream defaultProtocol
	bind s (SockAddrUnix namedpipe)
	listen s 2
	forever $ do
		(client, _addr) <- accept s
		h <- socketToHandle client ReadWriteMode
		hSetBuffering h LineBuffering
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
			case v of
				Done _ -> noop
				_ -> runwriter
		writer <- async runwriter

		let mkreader t from = maybe noop (const $ mkreader t from) 
			=<< catchMaybeIO (writeChan chan . t =<< hGetLine from)
		void $ concurrently
			(mkreader StdoutLine outh)
			(mkreader StderrLine outh)

		writeChan chan . Done =<< waitForProcess pid

		wait writer

		hClose outh
		hClose errh
		hClose h

simpleShClient :: FilePath -> String -> [String] -> ([Resp] -> IO a) -> IO a
simpleShClient namedpipe cmd params handler = do
	s <- socket AF_UNIX Stream defaultProtocol
	connect s (SockAddrUnix namedpipe)
	h <- socketToHandle s ReadWriteMode
	hSetBuffering h LineBuffering
	hPutStrLn h $ show $ Cmd cmd params
	resps <- catMaybes . map readish . lines <$> hGetContents h
	hClose h `after` handler resps

getStdout :: Resp -> Maybe String
getStdout (StdoutLine s) = Just s
getStdout _ = Nothing

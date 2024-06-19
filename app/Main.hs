{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}


module Main where

import qualified UnliftIO.Exception as E
import Data.ByteString.Builder (byteString)
import qualified Data.ByteString.Builder as BSB
import Network.HTTP.Types (ok200)
import Network.Run.TCP (runTCPServer) -- network-run

import Network.HTTP2.Server as Server

import qualified Data.ByteString.Char8 as C8
import Network.HTTP.Types
import Network.Run.TCP (runTCPClient) -- network-run
import UnliftIO.Async -- unliftio
import qualified UnliftIO.Exception as E -- unliftio
import Data.ByteString as BS

import Network.HTTP2.Client as Client
import Data.String
import Control.Concurrent
import Control.Monad

import System.Environment

main :: IO ()
main = do
    args <- getArgs
    _ <- forkIO $ myServer
    threadDelay 1000000
    runClient 100


myServer :: IO ()
myServer = runTCPServer (Just serverName) "12080" runHTTP2Server
  where
    runHTTP2Server s = E.bracket (allocSimpleConfig s 4096)
                                 freeSimpleConfig
                                 (\config -> Server.run defaultServerConfig config server)
    server _req _aux sendResponse = sendResponse response []
      where
        response = responseBuilder ok200 header body
        header = [(fromString "Content-Type", C8.pack "text/plain")] :: ResponseHeaders
        body = BSB.string8 "Hello, world!\n"


serverName :: String
serverName = "127.0.0.1"

runClient :: Int -> IO ()
runClient requests = runTCPClient serverName "12080" $ runHTTP2Client serverName
  where
    cliconf host = defaultClientConfig { authority = host }
    runHTTP2Client host s = E.bracket (allocSimpleConfig s 4096)
                                      freeSimpleConfig
                                      (\conf -> Client.run (cliconf host) conf client)
    client :: Client ()
    client sendRequest _aux = forM_ [0..requests :: Int] $ \i -> do
        when (i `mod` 50 == 0) $ print i
        let req0 = requestNoBody methodGet (C8.pack "/") []
            client0 = sendRequest req0 $ \rsp -> do
                -- print rsp
                !_r <- getResponseBodyChunk rsp :: IO C8.ByteString
                return ()
                -- C8.putStrLn r
            req1 = requestNoBody methodGet (C8.pack "/foo") []
            client1 = sendRequest req1 $ \rsp -> do
                -- print rsp
                !_r <- getResponseBodyChunk rsp
                return ()
        ex <- E.try $ concurrently_ client0 client1
        case ex of
          Left  e  -> print (e :: HTTP2Error)
          Right () -> return ()
                      --putStrLn "OK"
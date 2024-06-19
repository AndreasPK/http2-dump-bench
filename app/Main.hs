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
import Debug.Trace

main :: IO ()
main = do
    _ <- forkIO $ myServer
    threadDelay 1000000
    runClient 111

{-# NOINLINE bs_server_response #-}
bs_server_response :: ByteString
bs_server_response = BS.concat $ Prelude.replicate 3 $ BS.replicate 2048 66

myServer :: IO ()
myServer = runTCPServer (Just serverName) "12080" runHTTP2Server
  where
    runHTTP2Server s = E.bracket (allocSimpleConfig s 2048)
                                 freeSimpleConfig
                                 (\config -> Server.run defaultServerConfig config server)
    server _req _aux sendResponse = sendResponse response []
      where
        response = responseBuilder ok200 header body
        header = [(fromString "Content-Type", C8.pack "text/plain")] :: ResponseHeaders
        body = byteString bs_server_response


serverName :: String
serverName = "127.0.0.1"

runClient :: Int -> IO ()
runClient requests = runTCPClient serverName "12080" $ runHTTP2Client serverName
  where
    cliconf host = defaultClientConfig { authority = host }
    runHTTP2Client host s = E.bracket (allocSimpleConfig s 2048)
                                      freeSimpleConfig
                                      (\conf -> Client.run (cliconf host) conf client)
    client :: Client ()
    client sendRequest _aux = forM_ [0..requests :: Int] $ \i -> do
        when (i `mod` 50 == 0) $ print i
        print i
        let req0 = requestNoBody methodGet (C8.pack "/") []
            -- Runtime is essentially linear to the number of invocations of this.
            -- Sending more data by comparison makes hardly a dent.
            client0 = sendRequest (req0) $ \rsp -> do

                let readAll bytes_read reads = do
                      -- print bytes_read
                      chunk <- getResponseBodyChunk rsp
                      if BS.null chunk then return (bytes_read, reads) else readAll (bytes_read + BS.length chunk) (reads+1 :: Int)
                -- !_ <- return $! rsp
                -- chunk <- getResponseBodyChunk rsp
                (bytes_read,reads) <- readAll 0 0
                when (i `mod` 50 == 0) $ print (bytes_read,reads)
                return ()

        ex <- E.try $ client0
        case ex of
          Left  e  -> print (e :: HTTP2Error)
          Right () -> return ()
        -- ex <- E.try $ client0
        -- case ex of
        --   Left  e  -> print (e :: HTTP2Error)
        --   Right () -> return ()

{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Concurrent
import Control.Monad
import qualified Data.ByteString as S
import Data.ByteString(ByteString)
import qualified Data.ByteString.Char8 as C

-- network-run
import Network.Run.TCP (runTCPServer, runTCPClient)

-- network
import Network.Socket.ByteString (recv, sendAll)
import Network.Socket
import qualified Data.ByteString as BS

recvAll :: Socket -> Int -> IO ByteString
recvAll s n = do
    go n S.empty
    where
        go 0 bs = pure bs
        go n bs = do
            -- print "recv"
            bs' <- recv s (min 10 n)
            when (S.null bs') $ error "connection terminated"
            go (n - (S.length bs')) (bs <> bs')


main :: IO ()
main = do
    _ <- forkIO server
    threadDelay 10_00000
    client 10000

server_ms :: ByteString
server_ms = "Hello from server" :: ByteString

server :: IO ()
server = runTCPServer (Just "127.0.0.1") "3000" talk
  where
    talk s = do
        msg <- recvAll s 25
        -- print msg
        let server_msg =
                -- size
                BS.singleton (fromIntegral $ BS.length server_ms) <>
                -- payload
                server_ms
        -- print server_msg
        forM_ [0..1000 :: Int] $ \i -> do
            -- print $ S.drop 1 $ server_msg
            sendAll s server_msg
            threadDelay 100

        print "done - server"
        threadDelay 5_000_000


client :: Int -> IO ()
client n = runTCPClient "127.0.0.1" "3000" (myClient)
  where
    myClient s = do
      let msg_client = (S.replicate 25 65)
      print msg_client
      sendAll s msg_client
      threadDelay 100_000
      loop (Just 0) s

    loop Nothing s = putStrLn "done"
    loop (Just n) s = do
        print n
        header <- recv s 1
        -- putStrLn $ "header:" ++ show header
        -- print $ S.null header
        when (not $ S.null header) $ do
          let n_msg = fromIntegral (S.index header 0)
          msg <- recvAll s n_msg
          -- print msg
          loop (Just $ n+1) s

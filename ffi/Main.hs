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
import Data.IORef
import System.CPUTime
import Data.Time
import GHC.IO (unsafePerformIO)
import Data.Char
import Data.Bits
import Debug.Trace

recvAll :: Socket -> Int -> IO ByteString
recvAll s n = do
    go n S.empty
    where
        go 0 !bs = pure bs
        go n bs = do
            -- print "recv"
            bs' <- recv s (min 2048 n)
            when (S.null bs') $ error "connection terminated"
            go (n - (S.length bs')) (bs <> bs')

main :: IO ()
main = do
    _ <- forkIO server
    threadDelay 10_00000
    client 10000

server :: IO ()
server = runTCPServer (Just "127.0.0.1") "3001" (prep talk)
  where
    prep talk s = do
      setSockOpt s NoDelay (1::Int)
      talk s
    talk s = do
        forM_ [0..100 :: Int] $ \i -> do
            exchange "server:" (readBytes s) (sendBytes s) (sendBytes s)

        print "done - server"
        threadDelay 5_00_000

mkMeasure :: IO (IO NominalDiffTime)
mkMeasure = do
  tref <- newIORef =<< getCurrentTime
  return $ do
    old <- readIORef tref
    new <- getCurrentTime
    writeIORef tref new
    return (new `diffUTCTime` old)

{-# NOINLINE measure #-}
measure = unsafePerformIO $ mkMeasure

{-# NOINLINE measureEvery #-}
measureEvery = unsafePerformIO $ mkMeasure

-- Simulates a server/client exchange
-- args are: client send, client read, client read with -1
{-# INLINE exchange #-}
exchange :: [Char] -> (Int -> IO a1) -> (Int -> IO a2) -> (Int -> IO b) -> IO b
exchange side sb rb rbm = do
        traceEventIO ("rb:"++side ++ show 2)
        rb 2

        traceEventIO ("rb:"++side ++ show 9)
        rb 9

        traceEventIO ("sb:"++side ++ show 13)
        sb 13

        traceEventIO ("rb:"++side ++ show 2)
        rb 2

        traceEventIO ("rb:"++side ++ show 9)
        rb 9

        traceEventIO ("sb:"++side ++ show 2028)
        sb 2028

        traceEventIO ("rb:"++side ++ show 9)
        rbm 9

client :: Int -> IO ()
client n = do
  runTCPClient "127.0.0.1" "3001" (myClient)
  where
    myClient s = do
      -- let msg_client = (S.replicate 25 65)
      -- print msg_client
      -- sendAll s msg_client
      -- threadDelay 100_000
      setSockOpt s NoDelay ((1::Int))
      loop (Just 0) s

    loop Nothing s = putStrLn "done"
    loop (Just n) s = do
        msg_diff <- measureEvery
        traceEventIO $ "msgReadTime:" ++ show msg_diff

        when (mod n 50 == 0) $ do
          t_diff <- measure
          putStrLn $ "mod 50 iterations took:" ++ show t_diff

        let
            sb = sendBytes s
            rb = void . readBytes s
            rbm = void . readBytes s
        exchange "client:" sb rb rbm
        loop (Just $ n+1) s

sendBytes :: Socket -> Int -> IO ()
sendBytes s n = sendAll s (BS.replicate n $ fromIntegral $ ord 's')

readBytes :: Socket -> Int -> IO ByteString
readBytes s n = recvAll s n
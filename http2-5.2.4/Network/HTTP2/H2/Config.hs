module Network.HTTP2.H2.Config where

import Data.IORef
import Foreign.Marshal.Alloc (free, mallocBytes)
import Network.HTTP.Semantics.Client
import Network.Socket
import Network.Socket.ByteString (sendAll)
import qualified System.TimeManager as T

import Network.HPACK
import Network.HTTP2.H2.Types
import Debug.Trace (traceEventIO)
import qualified Data.ByteString as BS
import System.TimeIt

-- | Making simple configuration whose IO is not efficient.
--   A write buffer is allocated internally.
allocSimpleConfig :: Socket -> BufferSize -> IO Config
allocSimpleConfig s bufsiz = do
    buf <- mallocBytes bufsiz
    ref <- newIORef Nothing
    timmgr <- T.initialize $ 30 * 1000000
    mysa <- getSocketName s
    peersa <- getPeerName s
    let config =
            Config
                { confWriteBuffer = buf
                , confBufferSize = bufsiz
                , confSendAll = \bs -> do
                    traceEventIO ("sendAll_start:" ++ show (BS.length bs))
                    (time,r) <- timeItT $ sendAll s bs
                    traceEventIO ("sendAll_end:" ++ show (BS.length bs, time))
                    return r
                , confReadN = \n -> do
                    traceEventIO ("confReadN_start:" ++ show n)
                    (time,r) <- timeItT $ defaultReadN s ref n
                    traceEventIO ("confReadN_end:" ++ show (BS.length r, time))
                    return r

                , confPositionReadMaker = defaultPositionReadMaker
                , confTimeoutManager = timmgr
                , confMySockAddr = mysa
                , confPeerSockAddr = peersa
                }
    return config

-- | Deallocating the resource of the simple configuration.
freeSimpleConfig :: Config -> IO ()
freeSimpleConfig conf = do
    free $ confWriteBuffer conf
    T.killManager $ confTimeoutManager conf

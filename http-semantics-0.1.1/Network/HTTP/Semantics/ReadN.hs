{-# LANGUAGE LambdaCase #-}

module Network.HTTP.Semantics.ReadN (
    -- * Reading n bytes
    ReadN,
    defaultReadN,
)
where

import qualified Data.ByteString as B
import Data.IORef
import Network.Socket
import qualified Network.Socket.ByteString as N
import Debug.Trace (traceEventIO)

-- | Reading n bytes.
type ReadN = Int -> IO B.ByteString

tryTakeIORef :: IORef (Maybe a) -> IO (Maybe a)
tryTakeIORef ref = atomicModifyIORef ref (\old -> (Nothing, old))

putIORef :: IORef (Maybe a) -> a -> IO ()
putIORef ref x = atomicModifyIORef ref (\case
                                            Nothing -> (Just x, ())
                                            Just _ -> error "put putt"
                                        )

-- | Naive implementation for readN.
-- returns the requested number of bytes or nothing
defaultReadN :: Socket -> IORef (Maybe B.ByteString) -> ReadN
defaultReadN _ _ 0 = return B.empty
defaultReadN s ref n = do
    mbs <- tryTakeIORef ref
    traceEventIO $ "defaultReadN:" ++ show (n, mbs)
    -- mbs <- readIORef ref
    -- writeIORef ref Nothing
    case mbs of
        Nothing -> do
            bs <- N.recv s n
            if B.null bs
                then return B.empty
                else
                    if B.length bs == n
                        then return bs
                        -- Not enough data
                        else loop bs
        Just bs
            | B.length bs == n -> return bs
            | B.length bs > n -> do
                let (bs0, bs1) = B.splitAt n bs
                putIORef ref (bs1)
                return bs0
            | otherwise -> loop bs
  where
    loop bs = do
        -- bytes still to read
        let n' = n - B.length bs
        bs1 <- N.recv s n'
        -- no new data
        if B.null bs1
            then return B.empty
            else do
                let bs2 = bs `B.append` bs1
                if B.length bs2 == n
                    then return bs2
                    else loop bs2


-- -- | Naive implementation for readN.
-- defaultReadN :: Socket -> IORef (Maybe B.ByteString) -> ReadN
-- defaultReadN _ _ 0 = return B.empty
-- defaultReadN s ref n = do
--     mbs <- readIORef ref
--     writeIORef ref Nothing
--     case mbs of
--         Nothing -> do
--             bs <- N.recv s n
--             if B.null bs
--                 then return B.empty
--                 else
--                     if B.length bs == n
--                         then return bs
--                         else loop bs
--         Just bs
--             | B.length bs == n -> return bs
--             | B.length bs > n -> do
--                 let (bs0, bs1) = B.splitAt n bs
--                 writeIORef ref (Just bs1)
--                 return bs0
--             | otherwise -> loop bs
--   where
--     loop bs = do
--         let n' = n - B.length bs
--         bs1 <- N.recv s n'
--         if B.null bs1
--             then return B.empty
--             else do
--                 let bs2 = bs `B.append` bs1
--                 if B.length bs2 == n
--                     then return bs2
--                     else loop bs2

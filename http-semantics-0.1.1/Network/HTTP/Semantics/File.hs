module Network.HTTP.Semantics.File (
    -- * Position read
    PositionRead,
    PositionReadMaker,
    Sentinel (..),
    defaultPositionReadMaker,
) where

import System.IO

import Network.ByteOrder
import Network.HTTP.Semantics

-- | Position read for files.
type PositionRead = FileOffset -> ByteCount -> Buffer -> IO ByteCount

-- | Making a position read and its closer.
type PositionReadMaker = FilePath -> IO (PositionRead, Sentinel)

--- | Manipulating a file resource.
data Sentinel
    = -- | Closing a file resource. Its refresher is automatiaclly generated by
      --   the internal timer.
      Closer (IO ())
    | -- | Refreshing a file resource while reading.
      --   Closing the file must be done by its own timer or something.
      Refresher (IO ())

-- | Position read based on 'Handle'.
defaultPositionReadMaker :: PositionReadMaker
defaultPositionReadMaker file = do
    hdl <- openBinaryFile file ReadMode
    return (pread hdl, Closer $ hClose hdl)
  where
    pread :: Handle -> PositionRead
    pread hdl off bytes buf = do
        hSeek hdl AbsoluteSeek $ fromIntegral off
        fromIntegral <$> hGetBufSome hdl buf (fromIntegral bytes)

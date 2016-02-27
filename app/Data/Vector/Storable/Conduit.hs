module Data.Vector.Storable.Conduit(
   sinkFileLengthPrefixedVector
   , sinkFileLengthPrefixedVector_
) where

import System.IO
import Data.IORef
import Data.Conduit
import Data.Conduit.Blaze(builderToByteString)
import qualified Blaze.ByteString.Builder as Blaze
import qualified Data.Conduit.Combinators as CC
import qualified Data.ByteString.Builder as BB
import Control.Monad.Trans.Resource
import Control.Monad(void)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Foreign.Storable

sinkFileLengthPrefixedVector_ :: (MonadResource m, Storable a)
                                  => FilePath
                                  -> Sink a m ()
sinkFileLengthPrefixedVector_ = void . sinkFileLengthPrefixedVector

-- | serialize stream of storables to file
--   and prefix file with 8 bytes LE number of elements
sinkFileLengthPrefixedVector :: (MonadResource m, Storable a)
                                  => FilePath
                                  -> Sink a m Int
sinkFileLengthPrefixedVector fileName = do
  elemCounter <- liftIO $ newIORef 0
  bracketP openFileAndReservePrefix
           (closeAndWritePrefix elemCounter)
           (serialize elemCounter)
  val <- liftIO $ readIORef elemCounter
  return $ fromIntegral val
    where openFileAndReservePrefix = do
            h <- openBinaryFile fileName WriteMode
            putLength h 0
            return h
          closeAndWritePrefix v h = hSeek h AbsoluteSeek 0
                                    >> readIORef v >>= putLength h >> hClose h
          serialize v h = passthroughSink CC.length (liftIO . writeIORef v)
                          =$= CC.map Blaze.fromStorable
                          =$= builderToByteString
                          =$= CC.sinkHandle h
          putLength h = BB.hPutBuilder h . BB.word64LE

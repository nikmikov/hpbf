module Data.Vector.Storable.MMap(
    mmapLengthPrefixedMVector
    , mmapLengthPrefixedVector
) where

import Foreign.Storable
import Foreign.Ptr
import Foreign.ForeignPtr
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as MV
import qualified Data.Vector.Storable.Internal as MVInternal
--import Data.Int
import Data.Binary
import Data.Binary.Get
import qualified Data.ByteString.Internal as BS(fromForeignPtr)
import qualified Data.ByteString.Lazy as BSL


import qualified System.IO.MMap as MMap

mmapLengthPrefixedFile :: Storable a =>
                          FilePath -> MMap.Mode -> IO (Int, ForeignPtr a)
mmapLengthPrefixedFile fp mode = do
  (rawPtr, _, _) <- MMap.mmapFileForeignPtr fp mode Nothing
  let len = runGet getWord64le $
            BSL.fromStrict $ BS.fromForeignPtr (rawPtr::ForeignPtr Word8) 0 8
  let dataPtr = MVInternal.updPtr (`plusPtr` 8) rawPtr -- will keep finalizer
  return (fromIntegral len, castForeignPtr dataPtr)


mmapLengthPrefixedMVector :: Storable a => FilePath -> IO (MV.IOVector a)
mmapLengthPrefixedMVector fp = do
  (len, ptr) <- mmapLengthPrefixedFile fp MMap.ReadWrite
  return $ MV.unsafeFromForeignPtr0 ptr len



mmapLengthPrefixedVector :: Storable a => FilePath -> IO (V.Vector a)
mmapLengthPrefixedVector fp = do
  (len, ptr) <- mmapLengthPrefixedFile fp MMap.ReadOnly
  return $ V.unsafeFromForeignPtr0 ptr len

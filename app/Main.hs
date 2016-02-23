{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment(getArgs)

import Data.OSMPBF.Decoder
import Data.OSMPBF.Primitives
import Data.Conduit
import Data.Conduit.Blaze(builderToByteString)
import qualified Data.Conduit.Combinators as CC
import Control.Monad.Trans.Resource
import Data.Text(Text)
import qualified Data.HashMap.Strict as M
import Data.Maybe
import Data.Int
import Data.Binary
import Data.Binary.Put
import Data.Binary.Get
import Data.Conduit.Serialization.Binary(conduitEncode)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Exception.Base(bracket)
import qualified Data.Vector.Storable.Mutable as MV
import qualified Data.Vector.Storable.Internal as MVInternal
import qualified Data.Vector.Algorithms.Intro as VS

import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS(fromForeignPtr)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Builder as BB

import qualified Blaze.ByteString.Builder as Blaze

import Foreign.Ptr
import Foreign.Storable
import Data.IORef
--import Foreign.ForeignPtr.Unsafe
import Foreign.ForeignPtr
import System.IO.MMap
import System.IO
import Data.Function(on)
import Data.Foldable


isWay :: PBFPrimitive -> Bool
isWay (PBFWay _) = True
isWay _ = False

isNode :: PBFPrimitive -> Bool
isNode (PBFNode _) = True
isNode _ = False

getNode :: PBFPrimitive -> Maybe Node
getNode (PBFNode n) = Just n
getNode _ = Nothing

getWay :: PBFPrimitive -> Maybe Way
getWay (PBFWay n) = Just n
getWay _ = Nothing


data HpbfNode = HpbfNode {
      hpbfNodeId :: Int64
    , hpbfLat :: Int32
    , hpbfLon :: Int32
    } deriving (Show)

data HpbfWay = HpbfWay {
      hpbfWayId :: Int64
    , hpbfWayNodesVec :: [Int64]
    } deriving (Show)

toHpbfWay :: Way -> HpbfWay
toHpbfWay = HpbfWay
            <$> wayId
            <*> wayRefs

toHpbfNode :: Node -> HpbfNode
toHpbfNode = HpbfNode
               <$> nodeId
               <*> (convertCoord . fst . nodeCoord)
               <*> (convertCoord . snd . nodeCoord)

convertCoord :: Int64 -> Int32
convertCoord n = fromIntegral (n `div` 100)


instance Binary HpbfNode where
    put n = putInt64host (hpbfNodeId n)
            *> putInt32host (hpbfLat n) *> putInt32host (hpbfLon n)
    get = fail "" -- hpbfNodeId <*> hpbfLat <*> hpbfLon


instance Binary HpbfWay where
    put n = putInt64host (hpbfWayId n)
            *> putWord32host (fromIntegral $ length $ hpbfWayNodesVec n)
            *> mapM_ putInt64host (hpbfWayNodesVec n)
    get = fail "" -- hpbfNodeId <*> hpbfLat <*> hpbfLon


instance Storable HpbfNode where
    sizeOf _ = 16
    alignment _ = alignment (undefined :: Word32)
    peek ptr = HpbfNode
           <$> (`peekByteOff` 0) ptr
           <*> (`peekByteOff` 8) ptr
           <*> (`peekByteOff` 12) ptr
    poke ptr (HpbfNode id' lat lon) =
               (`pokeByteOff` 0) ptr id'
               *> (`pokeByteOff` 8) ptr lat
               *> (`pokeByteOff` 12) ptr lon

-- | returns true if tag with given key exists
tagExists :: Text -> [Tag] -> Bool
tagExists k t = isJust $ findTag k t

-- | find tag by key
findTag :: Text -> [Tag] -> Maybe Tag
findTag key = find ( (==) key . fst )

-- | get list of tags from primitive
tagsFromPrimitive :: PBFPrimitive -> [Tag]
tagsFromPrimitive (PBFWay w) = wayTags w
tagsFromPrimitive (PBFNode n) = nodeTags n
tagsFromPrimitive (PBFRelation r) = relTags r
tagsFromPrimitive (PBFHeader _) = []

findTagPrim :: Text -> PBFPrimitive -> Maybe Tag
findTagPrim k = findTag k . tagsFromPrimitive

type TagsFreq = M.HashMap Text Int

addPrim :: Text -> TagsFreq -> PBFPrimitive -> TagsFreq
addPrim k m p = case findTagPrim k p of
                  ( Just (_, v) ) -> M.insertWith (+) v 1 m
                  Nothing -> m



mmapVec :: Storable a => FilePath -> IO (MV.IOVector a)
mmapVec fp = do
  (rawPtr, _, _) <- mmapFileForeignPtr fp ReadWrite Nothing
  let len = runGet getWord32le $ BSL.fromStrict $ BS.fromForeignPtr (rawPtr::ForeignPtr Word8) 0 4
  let dataPtr = MVInternal.updPtr (`plusPtr` 4) rawPtr -- will keep finalizer
  return $ MV.unsafeFromForeignPtr0 (castForeignPtr dataPtr) (fromIntegral len )

sinkNodes :: MonadResource m =>  Sink PBFPrimitive m ()
sinkNodes = CC.concatMap getNode =$= CC.map toHpbfNode =$= serializeToLengthPrefixedVector' workNodesFile

-- | return number of elements in the stream
streamLength :: (Monad m, Integral i) => Sink a m i
streamLength = CC.foldl (\a _ -> a + 1) 0

sinkWays :: MonadResource m => Sink PBFPrimitive m ()
sinkWays = CC.concatMap getWay =$= CC.map toHpbfWay =$= serializeToLengthPrefixedVector workLinksFile

workNodesFile :: FilePath
workNodesFile = "_work.nodes"

workLinksFile :: FilePath
workLinksFile = "_work.links"

workNodesRefFile :: FilePath
workNodesRefFile = "_work.nodes_ref"

-- | serialize stream to file and prefix file with 4 bytes LE number of elements
serializeToLengthPrefixedVector :: (MonadResource m, Binary a) => FilePath -> Sink a m ()
serializeToLengthPrefixedVector fileName = do
  elemCounter <- liftIO $ newIORef 0
  bracketP openFileAndReservePrefix (closeAndWritePrefix elemCounter) (serialize elemCounter)
    where openFileAndReservePrefix = do
            h <- openBinaryFile fileName WriteMode
            putLength h 0
            return h
          closeAndWritePrefix v h = do
            hSeek h AbsoluteSeek 0
            len <- readIORef v
            putLength h len
            hClose h
          serialize v h = passthroughSink streamLength (liftIO . writeIORef v) =$= conduitEncode =$= CC.sinkHandle h
          putLength h = BB.hPutBuilder h . BB.word32LE

-- | serialize stream to file and prefix file with 4 bytes LE number of elements
serializeToLengthPrefixedVector' :: (MonadResource m, Storable a) => FilePath -> Sink a m ()
serializeToLengthPrefixedVector' fileName = do
  elemCounter <- liftIO $ newIORef 0
  bracketP openFileAndReservePrefix (closeAndWritePrefix elemCounter) (serialize elemCounter)
    where openFileAndReservePrefix = do
            h <- openBinaryFile fileName WriteMode
            putLength h 0
            return h
          closeAndWritePrefix v h = hSeek h AbsoluteSeek 0 >> readIORef v >>= putLength h >> hClose h
          serialize v h = passthroughSink streamLength (liftIO . writeIORef v)
                          =$= CC.map Blaze.fromStorable =$= builderToByteString =$= CC.sinkHandle h
          putLength h = BB.hPutBuilder h . BB.word32LE

-- | initial export of links and nodes from PBF
exportVectors :: FilePath -> IO()
exportVectors fileName =  runResourceT $ CC.sourceFile fileName
                          $$ conduitPbfToPrimitives
                          =$= getZipSink (ZipSink sinkNodes *> ZipSink sinkWays)

-- | populate nodes reference counter vector. will go throw the links vector
--   and increment counter by one in nodes_ref vector for every node which is a part of a link
--   Then when we dome we will have 4 categories of nodes, depending on ref_counter
--     0  - can be filtered out, not part of road network
--     1  - just a point of link geometry
--     2  - tentatively a junction, but need to analyze further if links can be joined into single
--     3+ - junction

main :: IO ()
main = do
  args <- getArgs
  let fileName = args !! 0
  putStrLn $ "Reading from " ++ fileName
  exportVectors fileName
  vec <- mmapVec workNodesFile
  let numNodes = MV.length vec
  putStrLn $ "Vector: " ++ show numNodes
  VS.sortBy (compare `on` hpbfNodeId) vec
  putStrLn "Creating nodes refcounter vector"
  runResourceT $
    CC.replicate numNodes (0::Word8) $$ serializeToLengthPrefixedVector' workNodesRefFile
  v <- MV.read vec 0
  print v
  --fin

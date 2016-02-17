{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment(getArgs)

import Data.OSMPBF.Decoder
import Data.OSMPBF.Primitives
import Data.Conduit
import qualified Data.Conduit.Combinators as CC
import Control.Monad.Trans.Resource
import Data.Text(Text)
import qualified Data.HashMap.Strict as M
import Data.Maybe
import Data.Int
import Data.Binary
import Data.Binary.Put
import Data.Conduit.Serialization.Binary(conduitEncode)
import qualified Data.Vector.Storable.Mutable as MV

import qualified Data.Vector.Algorithms.Intro as VS    

import Foreign.Storable
--import Foreign.ForeignPtr
import System.IO.MMap
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
           <$> (\hsc_ptr -> peekByteOff hsc_ptr 0) ptr
           <*> (\hsc_ptr -> peekByteOff hsc_ptr 8) ptr
           <*> (\hsc_ptr -> peekByteOff hsc_ptr 12) ptr
    poke ptr (HpbfNode id' lat lon) =
               (\hsc_ptr -> pokeByteOff hsc_ptr 0) ptr id'
               *> (\hsc_ptr -> pokeByteOff hsc_ptr 8) ptr lat
               *> (\hsc_ptr -> pokeByteOff hsc_ptr 12) ptr lon

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

type Vec = MV.IOVector HpbfNode

mmapVec :: FilePath -> IO Vec
mmapVec fp = do
  (fptr, _, sz) <- mmapFileForeignPtr fp ReadWrite Nothing
  return $ MV.unsafeFromForeignPtr0 fptr (sz `div` 16)


sinkNodes :: MonadResource m => Sink PBFPrimitive m ()       
sinkNodes = CC.concatMap getNode
          =$= CC.map toHpbfNode
          =$= conduitEncode
          =$= CC.sinkFile "_work.nodes"

sinkWays :: MonadResource m => Sink PBFPrimitive m ()              
sinkWays = CC.concatMap getWay
          =$= CC.filter (tagExists "highway" . wayTags)
          =$= CC.map toHpbfWay
          =$= conduitEncode
          =$= CC.sinkFile "_work.ways"            
         
         
main :: IO ()
main = do
  args <- getArgs
  let fileName = args !! 0
  putStrLn $ "Reading from " ++ fileName
  runResourceT $
               CC.sourceFile fileName
                   $$ conduitPbfToPrimitives
                   =$= (getZipSink $ ZipSink sinkNodes  <* ZipSink sinkWays)

  putStrLn  "Sorting nodes"                     
  vec <- mmapVec "berlin.nodes"
  VS.sortBy (compare `on` hpbfNodeId) vec
  v <- MV.read vec 0
  print v

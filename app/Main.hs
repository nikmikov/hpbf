{-# LANGUAGE OverloadedStrings #-}

module Main(main) where

import System.Environment(getArgs)

import Data.OSMPBF.Decoder
import Data.OSMPBF.Primitives(PBFPrimitive)
import qualified Data.OSMPBF.Primitives as PBF
import Data.Conduit
import qualified Data.Conduit.Combinators as CC
import qualified Data.Conduit.List as CL(mapAccum)
import Control.Monad.Trans.Resource
import Control.Monad(void)
import qualified Data.Map.Strict as M
import Data.Int
import Data.Binary

import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as MV
import qualified Data.Vector.Algorithms.Intro as VS
import qualified Data.RoadGraph.Datatypes as RG
import qualified Data.RoadGraph.Internal.StorableData as RGI
import Data.Vector.Storable.MMap
import Data.Vector.Storable.Conduit
import qualified Data.Vector.Storable.Search as VSearch
import Foreign.Storable
import Data.Function(on)

toOsmNode :: PBF.Node -> RGI.OsmNode
toOsmNode = RGI.OsmNode
               <$> PBF.nodeId
               <*> (convertCoord . fst . PBF.nodeCoord)
               <*> (convertCoord . snd . PBF.nodeCoord)

-- | coordinate conversion fron nanoseconds with precision loss
convertCoord :: Int64 -> Int32
convertCoord n = fromIntegral (n `div` 100)

-- | Write vectors of OsmNode to disk
--   return vector length
sinkOsmNodes :: MonadResource m => Sink PBFPrimitive m Int
sinkOsmNodes = CC.concatMap PBF.getNode
             =$= CC.map toOsmNode
             =$= sinkFileLengthPrefixedVector RGI.workOsmNodesFile

-- | Write ways (Vector OsmWay) and link node reference vector (Vector Int64) to disk
--   return vector length of OsmWay vector
sinkOsmWays :: MonadResource m => Sink PBFPrimitive m Int
sinkOsmWays = CC.concatMap PBF.getWay
            =$= getZipSink (ZipSink sinkWays' *> ZipSink sinkNodeRef)
    where toOsmWay w a = let a' = a + (fromIntegral . length . PBF.wayRefs) w
                             w' = RGI.OsmWay (PBF.wayId w) a'
                         in (a', w')
          sinkWays' = void ( CL.mapAccum toOsmWay (0::Int32) )
                    =$= sinkFileLengthPrefixedVector RGI.workOsmWayFile
          sinkNodeRef = CC.concatMap PBF.wayRefs
                        =$= sinkFileLengthPrefixedVector RGI.workOsmWayNodesFile

-- | initial export of links and ways from PBF
--   will create 3 vectors: OsmNode, OsmWay, WayNodeReference : Int64
exportInitialVectors :: FilePath -> IO ()
exportInitialVectors fileName =  void $ runResourceT $ CC.sourceFile fileName
                                 $$ conduitPbfToPrimitives
                                 =$= getZipSink (ZipSink sinkOsmNodes *> ZipSink sinkOsmWays)

mmapNodes :: IO( V.Vector RGI.OsmNode )
mmapNodes = mmapLengthPrefixedVector RGI.workOsmNodesFile

mmapNodesMutable :: IO( MV.IOVector RGI.OsmNode )
mmapNodesMutable = mmapLengthPrefixedMVector RGI.workOsmNodesFile

mmapLinks :: IO( V.Vector RGI.OsmWay )
mmapLinks = mmapLengthPrefixedVector RGI.workOsmWayFile

mmapOsmWayNodes :: IO( V.Vector Int64 )
mmapOsmWayNodes = mmapLengthPrefixedVector RGI.workOsmWayNodesFile

mmapNodesRefCounter :: IO(V.Vector Word8)
mmapNodesRefCounter = mmapLengthPrefixedVector RGI.workOsmNodeRefCntFile

mmapNodesRefCounterMutable :: IO(MV.IOVector Word8)
mmapNodesRefCounterMutable = mmapLengthPrefixedMVector RGI.workOsmNodeRefCntFile

-- | populate nodes reference counter vector. will go throw the links vector
--   and increment counter by one in nodes_ref vector for every node which is a part of a link
--   Then when we done we will have 4 categories of nodes, depending on ref_counter
--     0  - can be filtered out, not part of road network
--     1  - just a point of link geometry
--     2  - tentatively a junction, but need to analyze further if links can be joined into single
--     3+ - junction
classifyNodes :: IO()
classifyNodes = do
  nodesVec <- mmapNodes
  nodesRefVec <- mmapOsmWayNodes
  nodesRefCounterVec <- mmapNodesRefCounterMutable
  V.mapM_ (incNodeCnt nodesVec nodesRefCounterVec ) nodesRefVec
   where incNodeCnt nodesVec nodesRefCounterVec i = do
                  ix <- case findNodeIndex i nodesVec of
                             (Just x) -> return x
                             _ -> fail $ "Unable to find node index: " ++ show i
                  let saturatedSucc a
                          | a == maxBound  = a
                          | otherwise      = succ a
                  MV.modify nodesRefCounterVec saturatedSucc ix


-- | given osmWay and nodes ref vector produce stream of (OsmWay, OsmWayNodeIds)
osmWaysWithNodeIds :: MonadResource m
                  => V.Vector RGI.OsmWay
                  -> V.Vector Int64
                  -> Source m (RGI.OsmWay, V.Vector Int64)
osmWaysWithNodeIds linksVec vec = void (CC.yieldMany linksVec =$= CL.mapAccum fn vec)
    where fn el v' = let nxtIdx = fromIntegral (RGI.osmWayNodeEnd el)
                         (vecView, vecRest) = V.splitAt nxtIdx v'
                     in ( vecRest, (el, vecView) )



sourceIxVector :: (Monad m, Storable a) => V.Vector a -> Source m (a, Int)
sourceIxVector v = CC.yieldMany v =$= void (CL.mapAccum acc 0)
    where acc a s = (succ s, (a, s) )


conduitGraphElements :: MonadResource m
                     => V.Vector RGI.OsmNode
                     -> Conduit (Word8, Int) m RG.Node
conduitGraphElements vec = CC.filter ( \(c, _) -> c > 1)
                           =$= CC.map (toRgNode . V.unsafeIndex vec . snd)
    where toPoint = RG.Point <$> RGI.osmNodeLat <*> RGI.osmNodeLon
          toRgNode = RG.Node <$> toPoint <*> const 0


-- | Create a junction vector
createJunctions :: IO Int
createJunctions = do
  nodesRefCounter <- mmapNodesRefCounter
  nodes <- mmapNodes
  runResourceT $
               sourceIxVector nodesRefCounter
               $$ CC.filter ( \(c, _) -> c > 1)
               =$= CC.map (V.unsafeIndex nodes . snd)
               =$= void (CL.mapAccum acc 0)
               =$= sinkFileLengthPrefixedVector RGI.workJunctionsFile
      where acc a s = (succ s, RGI.Junction a s)

createLinks :: IO()
createLinks = do
  linksVec <- mmapLinks
  linkNodes <- mmapOsmWayNodes
  l <- runResourceT $
    osmWaysWithNodeIds linksVec linkNodes
    $$ CC.length
  print l


findNodeIndex :: Int64 -> V.Vector RGI.OsmNode -> Maybe Int
findNodeIndex id' v = VSearch.find (compare `on` RGI.osmNodeId) v (RGI.OsmNode id' 0 0)

findJunctionIndex :: Int64 -> V.Vector RGI.Junction -> Maybe Int
findJunctionIndex id' v = VSearch.find cmp v (el id')
    where el id' = RGI.Junction (RGI.OsmNode id' 0 0) 0
          cmp = compare `on` (RGI.osmNodeId . RGI.junctionOsmNode)

sortNodesVec :: IO ()
sortNodesVec = do
  vec <- mmapNodesMutable
  let numNodes = MV.length vec
  VS.sortBy (compare `on` RGI.osmNodeId) vec

createNodesRefCounterVec :: IO()
createNodesRefCounterVec = do
  nodesVec <- mmapNodes
  let numNodes = V.length nodesVec
  runResourceT $
               CC.replicate numNodes (0::Word8)
               $$ sinkFileLengthPrefixedVector_ RGI.workOsmNodeRefCntFile

main :: IO ()
main = do
  args <- getArgs
  let fileName = args !! 0
  putStrLn $ "Reading from " ++ fileName
  exportInitialVectors fileName
  putStrLn "Sorting "
  sortNodesVec
  putStrLn "Creating nodes refcounter vector"
  createNodesRefCounterVec
  putStrLn "Classifying nodes"
  classifyNodes
  numNodes <- createJunctions
  putStrLn $ "Junctions created: " ++ show numNodes
  --v <- MV.read vec 0
  --print v
  --fin

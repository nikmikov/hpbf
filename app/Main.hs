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
import Data.Vector.Storable.MMap
import Data.Vector.Storable.Conduit
import qualified Data.Vector.Storable.Search as VSearch
import Foreign.Storable
import Data.Function(on)

data HpbfNode = HpbfNode {
      hpbfNodeId :: Int64
    , hpbfLat :: Int32
    , hpbfLon :: Int32
    } deriving (Show)

data HpbfJunction = HpbfJunction {
      hpbfNode :: HpbfNode
    , junctionIx :: Word32
    } deriving (Show)

data HpbfWay = HpbfWay {
      hpbfWayId :: Int64
    , hpbfWayNodeRef :: Int32 -- index of first node  in nodes_ref vector
    } deriving (Show)

toHpbfNode :: PBF.Node -> HpbfNode
toHpbfNode = HpbfNode
               <$> PBF.nodeId
               <*> (convertCoord . fst . PBF.nodeCoord)
               <*> (convertCoord . snd . PBF.nodeCoord)

convertCoord :: Int64 -> Int32
convertCoord n = fromIntegral (n `div` 100)

instance Storable HpbfWay where
    sizeOf _ = 12
    alignment _ = alignment (undefined :: Word32)
    peek ptr = HpbfWay
               <$> (`peekByteOff` 0) ptr
               <*> (`peekByteOff` 8) ptr
    poke ptr (HpbfWay id' nodeRef) =
                          (`pokeByteOff` 0) ptr id'
                          *> (`pokeByteOff` 8) ptr nodeRef

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


instance Storable HpbfJunction where
    sizeOf _ = 20
    alignment _ = alignment (undefined :: Word32)
    peek ptr = HpbfJunction
           <$> (`peekByteOff` 0) ptr
           <*> (`peekByteOff` 16) ptr
    poke ptr (HpbfJunction node ix) =
               (`pokeByteOff` 0) ptr node
               *> (`pokeByteOff` 16) ptr ix


sinkNodes :: MonadResource m =>  Sink PBFPrimitive m ()
sinkNodes = CC.concatMap PBF.getNode
          =$= CC.map toHpbfNode
          =$= sinkFileLengthPrefixedVector_ workNodesFile


sinkWays :: MonadResource m => Sink PBFPrimitive m ()
sinkWays = CC.concatMap PBF.getWay
         =$= getZipSink (ZipSink sinkWays' *> ZipSink sinkNodeRef)
    where toHpbfWay w a = let a' = a + (fromIntegral . length . PBF.wayRefs) w
                              w' = HpbfWay (PBF.wayId w) a'
                          in (a', w')
          sinkWays' = void ( CL.mapAccum toHpbfWay (0::Int32) )
                    =$= sinkFileLengthPrefixedVector_ workLinksFile
          sinkNodeRef = CC.concatMap PBF.wayRefs
                        =$= sinkFileLengthPrefixedVector_ workLinkNodesRefFile

workNodesFile :: FilePath
workNodesFile = "_work.nodes"

workJunctionsFile :: FilePath
workJunctionsFile = "_work.junctions"

workLinksFile :: FilePath
workLinksFile = "_work.links"

workNodesRefCntFile :: FilePath
workNodesRefCntFile = "_work.nodes_ref_cnt"

workLinkNodesRefFile :: FilePath
workLinkNodesRefFile = "_work.links_nodes_ref"



-- | initial export of links and nodes from PBF
exportVectors :: FilePath -> IO()
exportVectors fileName =  runResourceT $ CC.sourceFile fileName
                          $$ conduitPbfToPrimitives
                          =$= getZipSink (ZipSink sinkNodes *> ZipSink sinkWays)

mmapNodes :: IO( V.Vector HpbfNode )
mmapNodes = mmapLengthPrefixedVector workNodesFile

mmapLinks :: IO( V.Vector HpbfWay )
mmapLinks = mmapLengthPrefixedVector workNodesFile

mmapNodesMutable :: IO( MV.IOVector HpbfNode )
mmapNodesMutable = mmapLengthPrefixedMVector workNodesFile

mmapLinkNodesRef :: IO( V.Vector Int64 )
mmapLinkNodesRef = mmapLengthPrefixedVector workLinkNodesRefFile

mmapNodesRefCounterMutable :: IO(MV.IOVector Word8)
mmapNodesRefCounterMutable = mmapLengthPrefixedMVector workNodesRefCntFile

mmapNodesRefCounter :: IO(V.Vector Word8)
mmapNodesRefCounter = mmapLengthPrefixedVector workNodesRefCntFile


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
  nodesRefVec <- mmapLinkNodesRef
  nodesRefCounterVec <- mmapNodesRefCounterMutable
  putStrLn $ "Classifying nodes: " ++ show ( V.length nodesRefVec )
  V.mapM_ (incNodeCnt nodesVec nodesRefCounterVec ) nodesRefVec
   where incNodeCnt nodesVec nodesRefCounterVec i = do
                  let (Just ix) = findNodeIndex i nodesVec
                  let saturatedSucc a
                          | a == maxBound  = a
                          | otherwise      = succ a
                  MV.modify nodesRefCounterVec saturatedSucc ix



linkNodesRange :: MonadResource m
                  => V.Vector Int64 -> Conduit HpbfWay m (HpbfWay, V.Vector Int64 )
linkNodesRange vec = void (CL.mapAccum fn vec)
    where fn el v' = let nxtIdx = fromIntegral (hpbfWayNodeRef el)
                         (vecView, vecRest) = V.splitAt nxtIdx v'
                     in ( vecRest, (el, vecView) )



sourceIxVector :: (Monad m, Storable a) => V.Vector a -> Source m (a, Int)
sourceIxVector v = CC.yieldMany v =$= void (CL.mapAccum acc 0)
    where acc a s = (succ s, (a, s) )


conduitGraphElements :: MonadResource m
                     => V.Vector HpbfNode
                     -> Conduit (Word8, Int) m RG.Node
conduitGraphElements vec = CC.filter ( \(c, _) -> c > 1)
                           =$= CC.map (toRgNode . V.unsafeIndex vec . snd)
    where toPoint = RG.Point <$> hpbfLat <*> hpbfLon
          toRgNode = RG.Node <$> toPoint <*> const 0



createJunctions :: IO Int
createJunctions = do
  nodesRefCounter <- mmapNodesRefCounter
  nodes <- mmapNodes
  runResourceT $
               sourceIxVector nodesRefCounter
               $$ CC.filter ( \(c, _) -> c > 1)
               =$= CC.map (V.unsafeIndex nodes . snd)
               =$= void (CL.mapAccum acc 0)
               =$= sinkFileLengthPrefixedVector workJunctionsFile
      where acc a s = (succ s, (HpbfJunction a s) )

createLinks :: IO()
createLinks = do
  linksVec <- mmapLinks
  linkNodes <- mmapLinkNodesRef
  nodesRefCounter <- mmapNodesRefCounter
  l <- runResourceT $
    CC.yieldMany linksVec
    $$ linkNodesRange linkNodes
    =$= CC.length
  print l


addPrim :: Word8 -> M.Map Word8 Int -> M.Map Word8 Int
addPrim k = M.insertWith (+) k 1

printStat :: IO()
printStat = do
  nodesRefCounter <- mmapNodesRefCounter
  let m = V.foldr addPrim M.empty nodesRefCounter
  print m

findNodeIndex :: Int64 -> V.Vector HpbfNode -> Maybe Int
findNodeIndex id' v = VSearch.find (compare `on` hpbfNodeId) v (HpbfNode id' 0 0)

sortNodesVec :: IO ()
sortNodesVec = do
  vec <- mmapNodesMutable
  let numNodes = MV.length vec
  VS.sortBy (compare `on` hpbfNodeId) vec

createNodesRefCounterVec :: IO()
createNodesRefCounterVec = do
  nodesVec <- mmapNodes
  let numNodes = V.length nodesVec
  runResourceT $
               CC.replicate numNodes (0::Word8)
               $$ sinkFileLengthPrefixedVector_ workNodesRefCntFile

main :: IO ()
main = do
  args <- getArgs
  let fileName = args !! 0
  putStrLn $ "Reading from " ++ fileName
  exportVectors fileName
  putStrLn "Sorting "
  sortNodesVec
  putStrLn "Creating nodes refcounter vector"
  createNodesRefCounterVec
  classifyNodes
  printStat
  numNodes <- createJunctions
  print numNodes
  --v <- MV.read vec 0
  --print v
  --fin

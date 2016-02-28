--- | all sort of intermediate data required to build road graph
module Data.RoadGraph.Internal.StorableData

where

import Data.Int
import Data.Word
import Foreign.Storable

import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Search as VSearch
import Data.Function(on)

-- | Just a conversion from OSM node with lat/lon precision reduced
data OsmNode = OsmNode {
      osmNodeId :: Int64
    , osmNodeLat :: Int32
    , osmNodeLon :: Int32
    }


-- | OSM way with nodes ID stored in separate adjacency array
data OsmWay = OsmWay {
      osmWayId :: Int64
    , osmWayAttributes :: Int64
    , osmWayNodeEnd :: Int32 -- index of the first node for the NEXT element
    }


-- | Junction is a OsmNode which is referenced from 2+ OsmWay
data Junction = Junction {
      junctionOsmNode :: OsmNode -- corresponding OSM node
    , junctionId :: Word32       -- monotonic id [0..n-1]  where n - number of junctions
    }

workOsmNodesFile :: FilePath
workOsmNodesFile = "_work.osm_nodes"

workJunctionsFile :: FilePath
workJunctionsFile = "_work.junctions"

workOsmWayFile :: FilePath
workOsmWayFile = "_work.osm_way"

workOsmWayNodesFile :: FilePath
workOsmWayNodesFile = "_work.osm_way_nodes_ref"

workOsmNodeRefCntFile :: FilePath
workOsmNodeRefCntFile = "_work.osm_nodes_ref_cnt"



instance Storable OsmNode where
    sizeOf _ = 16
    alignment _ = alignment (undefined :: Word64)
    peek ptr = OsmNode
           <$> (`peekByteOff` 0) ptr
           <*> (`peekByteOff` 8) ptr
           <*> (`peekByteOff` 12) ptr
    poke ptr (OsmNode id' lat lon) =
        (`pokeByteOff` 0) ptr id'
        *> (`pokeByteOff` 8) ptr lat
        *> (`pokeByteOff` 12) ptr lon

instance Storable OsmWay where
    sizeOf _ = 20
    alignment _ = alignment (undefined :: Word64)
    peek ptr = OsmWay
               <$> (`peekByteOff` 0) ptr
               <*> (`peekByteOff` 8) ptr
               <*> (`peekByteOff` 16) ptr
    poke ptr (OsmWay id' attr nodeRef) =
        (`pokeByteOff` 0) ptr id'
        *> (`pokeByteOff` 8) ptr attr
        *> (`pokeByteOff` 16) ptr nodeRef

instance Storable Junction where
    sizeOf _ = 20
    alignment _ = alignment (undefined :: Word64)
    peek ptr = Junction
           <$> (`peekByteOff` 0) ptr
           <*> (`peekByteOff` 16) ptr
    poke ptr (Junction node ix) =
               (`pokeByteOff` 0) ptr node
               *> (`pokeByteOff` 16) ptr ix


-- | utility functions
findNodeIndex :: V.Vector OsmNode -> Int64 -> Maybe Int
findNodeIndex v id' = VSearch.find (compare `on` osmNodeId) v (OsmNode id' 0 0)

findJunctionIndex :: V.Vector Junction -> OsmNode  -> Maybe Int
findJunctionIndex v n = VSearch.find cmp v (Junction n 0)
    where cmp = compare `on` (osmNodeId . junctionOsmNode)

module Data.RoadGraph.Datatypes
where

import Data.Int
import Data.Word
import Foreign.Storable

import qualified Data.ByteString as BS

invalidId :: Word32
invalidId = maxBound

data Point = Point {
      pointLat :: Int32
    , pointLon :: Int32
      }

data Node = Node {
      nodeCoord :: Point
    , nodeLinksEndIx :: Word32
      }

type NodeLinkReference = Int32

data Link = Link {
      linkNodeIdForward  :: Word32
    , linkNodeIdBackward :: Word32
    , linkLengthPacked   :: Word16
    , linkAttributeIx    :: Word16
    }

type LinkPackedGeometry = BS.ByteString

type LinkPackedGeometryOffset = Word32

type LinkGeometry = [Point]

-- link geometry data is just a bytearray

data LinkAttributes = LinkAttributes{
      linkAttributes :: Int64
    }

--           Storable instances, carefuly handcrafted in Germany
instance Storable Point where
    sizeOf _ = 8
    alignment _ = alignment (undefined :: Word32)
    peek ptr = Point
               <$> (`peekByteOff` 0) ptr
               <*> (`peekByteOff` 4) ptr
    poke ptr (Point lat lon) = (`pokeByteOff` 0) ptr lat
                               *> (`pokeByteOff` 4) ptr lon

instance Storable Node where
    sizeOf _ = 12
    alignment _ = alignment (undefined :: Word32)
    peek ptr = Node
               <$> (`peekByteOff` 0) ptr
               <*> (`peekByteOff` 8) ptr
    poke ptr (Node coord linkRef) = (`pokeByteOff` 0) ptr coord
                                    *> (`pokeByteOff` 8) ptr linkRef

instance Storable Link where
    sizeOf _ = 12
    alignment _ = alignment (undefined :: Word32)
    peek ptr = Link
               <$> (`peekByteOff` 0) ptr
               <*> (`peekByteOff` 4) ptr
               <*> (`peekByteOff` 8) ptr
               <*> (`peekByteOff` 10) ptr
    poke ptr (Link fw bk lp la) =    (`pokeByteOff` 0)  ptr fw
                                  *> (`pokeByteOff` 4)  ptr bk
                                  *> (`pokeByteOff` 8)  ptr lp
                                  *> (`pokeByteOff` 10) ptr la

instance Storable LinkAttributes where
    sizeOf _ = 8
    alignment _ = alignment (undefined :: Word64)
    peek ptr = LinkAttributes
               <$> (`peekByteOff` 0) ptr
    poke ptr (LinkAttributes a) = (`pokeByteOff` 0)  ptr a
--
nodesFile :: FilePath
nodesFile = "graph.nodes"

linksFile :: FilePath
linksFile = "graph.links"

linkGeometryFile :: FilePath
linkGeometryFile = "graph.link_geometry"

linkGeometryOffsetFile :: FilePath
linkGeometryOffsetFile = "graph.link_geometry_offset"

-- helper functions: TODO: move them to separate module

-- | calculate linke length in meters
calculateLinkLengthMeters :: LinkGeometry -> Int
calculateLinkLengthMeters _ = 0

-- | pack link length in meters to Word16 with slight precision loss for long links
packLinkLength :: Int -> Word16
packLinkLength _ = 0

calculateLinkLengthPacked :: LinkGeometry -> Word16
calculateLinkLengthPacked  = packLinkLength . calculateLinkLengthMeters

-- | return link length in meters from packed format
unpackLinkLength :: Word16 -> Int
unpackLinkLength _ = undefined

-- | haveing full geometry pack just intermediate points (dropping first and last)
packLinkIntermediateGeometry :: LinkGeometry -> BS.ByteString
packLinkIntermediateGeometry _ = BS.empty

unpackLinkGeometry :: BS.ByteString -> LinkGeometry
unpackLinkGeometry _ = undefined

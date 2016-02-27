module Data.RoadGraph.Datatypes
where

import Data.Int
import Data.Word
import Foreign.Storable

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
    , linkAttributeIx   :: Word16
    }

type LinkPackedGeometryOffset = Word32

type LinkGeometry = [Point]

-- link geometry data is just a bytearray

data LinkAttributes = LinkAttributes{
      linkAttributes :: Int64
    }


--           Storable instances
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

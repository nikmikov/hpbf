module Data.Vector.Storable.Search(
  binarySearchBy, find)
where

import Data.Bits(shiftR)
import Foreign.Storable
import Data.Vector.Storable( (!?) )
import qualified Data.Vector.Storable as V

find :: Storable a => (a -> a -> Ordering) -> V.Vector a -> a -> Maybe Int
find cmp vec e = let ix = binarySearchBy cmp vec e
                     e' = vec !? ix
                 in case e' of
                      Just a ->  if cmp e a == EQ then Just ix else Nothing
                      Nothing -> Nothing


-- | copied from vector-algorithms to adapt for simple Vector
binarySearchBy :: Storable a
                  => (a -> a -> Ordering) -> V.Vector a -> a -> Int
binarySearchBy cmp vec e  = binarySearchByBounds cmp vec e 0 (V.length vec)


binarySearchByBounds :: Storable a
                        => (a -> a -> Ordering) -> V.Vector a -> a -> Int -> Int
                        -> Int
binarySearchByBounds cmp vec e = loop
    where
      loop l u
          | u <= l    = l
          | otherwise = let e' = vec `V.unsafeIndex` k
                        in case cmp e' e of
                             LT -> loop (k+1) u
                             EQ -> k
                             GT -> loop l     k
          where k = (u + l) `shiftR` 1

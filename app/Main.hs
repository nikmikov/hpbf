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
import Data.Foldable(find)
import Data.Maybe
import Data.Int
--import Data.Binary
import Data.Binary.Put

isWay :: PBFPrimitive -> Bool
isWay (PBFWay _) = True
isWay _ = False

isNode :: PBFPrimitive -> Bool
isNode (PBFNode _) = True
isNode _ = False

convertCoord :: Int64 -> Int32
convertCoord n = fromIntegral (n `div` 100)

putNode :: Node -> Put
putNode n = do
  putInt64le $ nodeId n
  putInt32le $ convertCoord $ fst $ nodeCoord $ n
  putInt32le $ convertCoord $ snd $ nodeCoord $ n

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




main :: IO ()
main = do
  args <- getArgs
  let fileName = args !! 0
  putStrLn $ "Reading from " ++ fileName
  res <- runResourceT $
               CC.sourceFile fileName
                   $$ conduitPbfToPrimitives
                   =$= CC.filter isWay
                   =$= CC.foldl (addPrim "highway") M.empty
  print res

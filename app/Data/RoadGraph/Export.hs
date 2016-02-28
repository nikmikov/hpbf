module Data.RoadGraph.Export (sinkExportLinks, sinkExportNodes)

where

import Data.Int
import Data.Word
import Data.Maybe

import Control.Monad.Trans.Resource
import Control.Monad(void)
import qualified Data.ByteString as BS
import qualified Data.Vector.Storable as V
import Data.Conduit
import qualified Data.Conduit.Combinators as CC
import qualified Data.Conduit.List as CL(mapAccum)

import qualified Data.RoadGraph.Datatypes as RG
import qualified Data.RoadGraph.Internal.StorableData as RGI

import Data.Vector.Storable.Conduit

linkAttrUnpacked :: FilePath
linkAttrUnpacked = "_work.attributes_unpacked"

data LinkComponent = LCLink           RG.Link
                   | LCLinkAttributes RG.LinkAttributes
                   | LCLinkGeometry   RG.LinkPackedGeometry

getLink :: LinkComponent -> Maybe RG.Link
getLink (LCLink a) = Just a
getLink _ = Nothing

getAttr :: LinkComponent -> Maybe RG.LinkAttributes
getAttr (LCLinkAttributes a) = Just a
getAttr _ = Nothing

getGeom :: LinkComponent -> Maybe RG.LinkPackedGeometry
getGeom (LCLinkGeometry a) = Just a
getGeom _ = Nothing

-- | split geometry
splitGeom :: Monad m =>  [RGI.OsmNode] -> m (RG.LinkGeometry, RGI.OsmNode, RGI.OsmNode)
splitGeom [] = fail "Empty geometry, at least 2 elements expected"
splitGeom [_] = fail "Single element geometry, at least 2 elements expected"
splitGeom xs = return (fmap toPoint xs, head xs, last xs)

toPoint :: RGI.OsmNode -> RG.Point
toPoint = RG.Point <$> RGI.osmNodeLat <*> RGI.osmNodeLon

createLinkComponents :: Monad m
                        => V.Vector RGI.Junction -> RGI.OsmWay -> [RGI.OsmNode]
                        -> m (RG.Link, RG.LinkAttributes, RG.LinkPackedGeometry)
createLinkComponents jv w ns = do ( fullGeom, sn, en ) <- splitGeom ns
                                  let attr = RG.LinkAttributes $ RGI.osmWayAttributes w
                                      geomPacked = RG.packLinkIntermediateGeometry fullGeom
                                      linkLengthPacked = RG.calculateLinkLengthPacked fullGeom
                                      jid (Just ix) = return $ RGI.junctionId $ V.unsafeIndex jv ix
                                      jid Nothing   = fail ""
                                  sn' <- jid $ RGI.findJunctionIndex jv sn
                                  en' <- jid $ RGI.findJunctionIndex jv en
                                  let link = RG.Link sn' en' linkLengthPacked 0
                                  return (link, attr, geomPacked)


splitLink :: [ (RGI.OsmNode, Word8) ] -> [[RGI.OsmNode]]
splitLink [] = fail "Empty splitLink arg"
splitLink ( (x',_):xs') = splitLink' xs' [x']
    where splitLink' [] ys = ys:[]
          splitLink' ( (n, i):xs ) ys = if i < 2
                                        then splitLink' xs (n:ys)
                                        else (n:ys):splitLink' xs [n]

conduitLinkComponents :: MonadResource m
                         => V.Vector RGI.OsmNode
                         -> V.Vector Word8
                         -> V.Vector RGI.Junction
                         -> Conduit (RGI.OsmWay, V.Vector Int64) m LinkComponent
conduitLinkComponents nv niv jv = awaitForever $ \(osmWay, wayNodes) ->
  do let nodes' :: [ (RGI.OsmNode, Word8) ]
         nodes' = V.foldr ( (:) . lookupPair . fromJust . RGI.findNodeIndex nv) [] wayNodes
         lookupPair i = (V.unsafeIndex nv i, V.unsafeIndex niv i)
         links' = splitLink nodes'
         streamLink l' = do (link, attr, geom) <- createLinkComponents jv osmWay l'
                            CC.yieldMany [LCLink link, LCLinkAttributes attr, LCLinkGeometry geom]
     mapM_ streamLink links'

sinkExportLinks :: MonadResource m
                         => V.Vector RGI.OsmNode
                         -> V.Vector Word8
                         -> V.Vector RGI.Junction
                         -> Sink (RGI.OsmWay, V.Vector Int64) m Int
sinkExportLinks nv niv jv = conduitLinkComponents nv niv jv
                          =$= getZipSink (ZipSink sinkLinks <* ZipSink sinkAttr <* ZipSink sinkGeom)
    where sinkLinks = CC.concatMap getLink
                      =$= sinkFileLengthPrefixedVector RG.linksFile
          sinkAttr = CC.concatMap getAttr
                      =$= sinkFileLengthPrefixedVector linkAttrUnpacked
          sinkGeom = CC.concatMap getGeom
                      =$= getZipSink (ZipSink (CC.sinkFile RG.linkGeometryFile)
                                      <* ZipSink sinkGeometryOffset)
          sinkGeometryOffset = void (CL.mapAccum accumOffset (0::Word32))
                               =$= sinkFileLengthPrefixedVector RG.linkGeometryOffsetFile
          accumOffset bs off = let newOffset = fromIntegral $ BS.length bs
                               in (newOffset, off)



sinkExportNodes :: MonadResource m
                   => Sink RGI.Junction m Int
sinkExportNodes = CC.map (toRgNode . RGI.junctionOsmNode)
                  =$= sinkFileLengthPrefixedVector RG.nodesFile
    where toRgNode = RG.Node <$> toPoint <*> const 0

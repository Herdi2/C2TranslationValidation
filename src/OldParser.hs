{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module OldParser () where

import Data.Char (isSpace, toLower)
import qualified Data.Map as M
import Data.SBV (Word32)
import Graph
import Text.XML.HXT.Core
import Text.XML.HXT.DOM.XmlNode (NTree)

normalize :: String -> String
normalize = dropWhile isSpace

-- | Parse the given graph GRAPH_NAME into a raw format containing nodes with id and dump_spec,
-- and edges with input index.
--
-- Parses the following format:
-- <graph name=GRAPH_NAME >
--  <nodes>
--    <node>
--    <node id=X>
--    </node>
--    <\node>
--  </nodes>
--  <edges>
--    <edge from=x to=y index=z/>
--  </edges>
-- </graph>
parseGraph :: String -> String -> IO RawGraph
parseGraph graphName xml = do
  let graph =
        readString [withValidate no] xml
          >>> deep (hasName "graph")
          -- NOTE: Removes initial whitespace since XML file has newline at start, e.g. "\nAfter Parsing"
          >>> hasAttrValue "name" ((==) graphName . normalize)
          >>> getChildren
  edges <-
    runX $
      graph
        >>> deep (hasName "edges")
        >>> deep (hasName "edge")
        >>> proc edge -> do
          from <- getAttrValue "from" -< edge
          to <- getAttrValue "to" -< edge
          toIndex <- getAttrValue "index" -< edge
          returnA
            -<
              ( read from :: NodeId,
                read to :: NodeId,
                read toIndex :: NodeId
              )
  nodes <-
    runX $
      graph
        >>> deep (hasName "nodes")
        >>> deep (hasName "node")
        >>> parseNode
  return $ RawGraph nodes edges

parseNode :: (ArrowChoice t, ArrowXml t) => t (NTree XNode) RawNode
parseNode =
  proc node -> do
    nodeId <- getAttrValue "id" -< node
    nodeName <- nodeProp "name" -< node
    nodeProps <- listA allProps -< node
    returnA -< RawNode nodeId (normalize nodeName) (M.fromList nodeProps)
  where
    nodeProp prop =
      deep (hasName "properties")
        >>> deep (hasName "p")
        >>> hasAttrValue "name" (== prop)
        >>> getChildren
        >>> getText
    allProps =
      deep (hasName "properties")
        >>> deep (hasName "p")
        >>> proc prop -> do
          name <- getAttrValue "name" -< prop
          value <- getText <<< getChildren -< prop
          returnA -< (normalize name, normalize value)

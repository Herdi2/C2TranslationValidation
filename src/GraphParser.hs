{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module GraphParser where

import Data.Char
import qualified Data.Map as M
import Text.XML.HXT.Core
import Text.XML.HXT.DOM.XmlNode (NTree)
import Verify

-- | (NodeId, Name, dump_spec)
type RawNode = (Int, String, String)

-- | Intermediate graph structure, after parsing but before fully constructing it.
-- May contain unsupported nodes.
data RawGraph = RawGraph
  { -- | (NodeId, Name, dump_spec)
    rawNodes :: [RawNode],
    -- | Edges and their toIndex (ordered input)
    -- (to, from, toIndex)
    rawEdges :: [(Int, Int, Int)]
  }
  deriving (Show)

normalize :: String -> String
normalize = map toLower . dropWhile isSpace

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
          >>> hasAttrValue "name" ((==) graphName . dropWhile isSpace)
          >>> getChildren
  edges <-
    runX $
      graph
        >>> deep (hasName "edges")
        >>> deep (hasName "edge")
        >>> proc edge -> do
          from <- getAttrValue "from" -< edge
          to <- getAttrValue "to" -< edge
          toIndex <- withDefault (getAttrValue "index") "0" -< edge
          returnA
            -<
              ( read from :: Int,
                read to :: Int,
                read toIndex :: Int
              )
  nodes <-
    runX $
      graph
        >>> deep (hasName "nodes")
        >>> deep (hasName "node")
        >>> parseNode edges
  return $ RawGraph nodes edges

parseNode :: (ArrowChoice t, ArrowXml t) => [(Int, Int, Int)] -> t (NTree XNode) (Int, String, String)
parseNode edges =
  proc node -> do
    nodeName <- nodeProp "name" -< node
    nodeId <- getAttrValue "id" -< node
    case (normalize nodeName) of
      "root" -> none -< (1, "", "")
      _ ->
        do
          dumpSpec <- nodeProp "dump_spec" -< node
          returnA -< (read nodeId :: Int, normalize nodeName, normalize dumpSpec)
  where
    nodeProp prop =
      deep (hasName "properties")
        >>> deep (hasName "p")
        >>> hasAttrValue "name" (== prop)
        >>> getChildren
        >>> getText

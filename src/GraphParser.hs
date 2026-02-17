module GraphParser (parseGraph) where

import Data.Char (isSpace)
import qualified Data.Map as M
import Graph
import Text.XML.Light

parseGraph :: String -> String -> Either String RawGraph
parseGraph graphName xml = do
  root <- maybe (Left "Failed to parse XML") Right $ parseXMLDoc xml
  graph <-
    maybe (Left $ "Graph not found: " <> graphName) Right $
      findGraphElement graphName root
  let edges = parseEdges graph
      nodes = parseNodes graph
  Right $ RawGraph nodes edges

-- | Find graph element given the name of the graph, e.g. "After Parsing"
findGraphElement :: String -> Element -> Maybe Element
findGraphElement graphName root =
  filterElement
    ( \el ->
        qName (elName el) == "graph"
          && findAttr (unqual "name") el == Just (dropWhile isSpace graphName)
    )
    root

-- | Given a <Graph> element, parses edges into list of (from, to, index)
parseEdges :: Element -> [(NodeId, NodeId, NodeId)]
parseEdges graph =
  [ (read from, read to, read mIndex)
  | edges <- findChildren (unqual "edges") graph,
    edge <- findChildren (unqual "edge") edges,
    Just from <- [findAttr (unqual "from") edge],
    Just to <- [findAttr (unqual "to") edge],
    Just mIndex <- [findAttr (unqual "index") edge]
  ]

-- | Given a <Graph> element, parses the nodes
parseNodes :: Element -> [RawNode]
parseNodes graph =
  [ parseRawNode nod
  | nodes <- findChildren (unqual "nodes") graph,
    nod <- findChildren (unqual "node") nodes
  ]

-- | Translate a <node> element given in <nodes> to an equivalent data structure in Haskell,
-- with all properties.
parseRawNode :: Element -> RawNode
parseRawNode el =
  RawNode
    { rawNodeId = trim $ maybe "" id $ findAttr (unqual "id") el,
      rawNodeName = trim $ maybe "" id $ M.lookup "name" props,
      rawNodeProps = props
    }
  where
    props =
      M.fromList
        [ (trim name, trim $ strContent p)
        | propsEl <- findChildren (unqual "properties") el,
          p <- findChildren (unqual "p") propsEl,
          Just name <- [findAttr (unqual "name") p]
        ]

trim :: String -> String
trim = dropWhile isSpace

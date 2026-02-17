module GraphBuilder (buildGraph) where

import Control.Monad.State.Lazy
import Data.List (sortOn)
import qualified Data.Map as M
import Graph

data ParseResult a
  = -- | Recognized values that are not needed
    Ignored
  | -- | Unrecognized values, marked as such
    Unsupported String
  | -- | Successful parse
    Parsed a

partitionParseResults :: [ParseResult a] -> ([String], [a])
partitionParseResults = go ([], [])
  where
    go acc [] = acc
    go acc (Ignored : rest) = go acc rest
    go (unsupported, parsed) (Unsupported s : rest) = go (s : unsupported, parsed) rest
    go (unsupported, parsed) (Parsed p : rest) = go (unsupported, p : parsed) rest

buildGraph :: RawGraph -> Either [String] Graph
buildGraph (RawGraph rNodes rEdges) =
  let (nodes, controlflow) = runState (mapM (buildNode rEdges) rNodes) M.empty
      (unsupported, parsedNodes) = partitionParseResults nodes
   in if null unsupported
        then
          Right $
            defaultGraph
              { nodeInfo = M.fromList parsedNodes,
                controlSuccessors = controlflow
              }
        else Left $ unsupported

type BuildM = State ControlSuccessors (ParseResult (NodeId, Node))

buildNode ::
  [(NodeId, NodeId, NodeId)] ->
  RawNode ->
  BuildM
buildNode rEdges (RawNode (read -> nodeId) nodeName nodeProps) =
  case nodeName of
    "Parm" ->
      case (M.lookup "type" nodeProps) of
        Just "int:" -> pure $ Parsed (nodeId, ParmI nodeId)
        Just "long:" -> pure $ Parsed (nodeId, ParmL nodeId)
        Just "float:" -> pure $ Parsed (nodeId, ParmF nodeId)
        Just "double:" -> pure $ Parsed (nodeId, ParmD nodeId)
        Just "control" -> pure $ Parsed (nodeId, ParmCtrl nodeId)
        Just "abIO" -> pure Ignored
        Just "rawptr:" -> pure Ignored
        Just "return_address" -> pure Ignored
        Just "memory" -> pure Ignored
        Just t -> pure $ Unsupported $ "Unsupported param of type: " <> t
        Nothing -> pure $ Unsupported $ "buildNode: Internal error, node property didn't contain \"type\""
    -- Constants
    "ConI" -> pure $ Parsed (nodeId, ConI $ read (nodeProps M.! "short_name"))
    "ConL" -> pure $ Parsed (nodeId, ConL $ read (nodeProps M.! "short_name"))
    "ConF" -> pure $ Parsed (nodeId, ConF $ fromInteger $ read (nodeProps M.! "short_name"))
    "ConD" -> pure $ Parsed (nodeId, ConD $ fromInteger $ read (nodeProps M.! "short_name"))
    -- Addition
    "AddI" -> pure $ arithmeticNode AddI nodeId rEdges
    "AddL" -> pure $ arithmeticNode AddL nodeId rEdges
    "AddF" -> pure $ arithmeticNode AddF nodeId rEdges
    "AddD" -> pure $ arithmeticNode AddD nodeId rEdges
    -- Subtraction
    "SubI" -> pure $ arithmeticNode SubI nodeId rEdges
    "SubL" -> pure $ arithmeticNode SubL nodeId rEdges
    "SubF" -> pure $ arithmeticNode SubF nodeId rEdges
    "SubD" -> pure $ arithmeticNode SubD nodeId rEdges
    -- Multiplication
    "MulI" -> pure $ arithmeticNode MulI nodeId rEdges
    "MulL" -> pure $ arithmeticNode MulL nodeId rEdges
    "MulF" -> pure $ arithmeticNode MulF nodeId rEdges
    "MulD" -> pure $ arithmeticNode MulD nodeId rEdges
    "MulHiL" -> pure $ arithmeticNode MulHiL nodeId rEdges
    -- Division
    "DivI" -> pure $ arithmeticNode DivI nodeId rEdges
    "DivL" -> pure $ arithmeticNode DivL nodeId rEdges
    "DivF" -> pure $ arithmeticNode DivF nodeId rEdges
    "DivD" -> pure $ arithmeticNode DivD nodeId rEdges
    -- Bitwise and
    "AndI" -> pure $ arithmeticNode AndI nodeId rEdges
    "AndL" -> pure $ arithmeticNode AndL nodeId rEdges
    -- Bitwise or
    "OrI" -> pure $ arithmeticNode OrI nodeId rEdges
    "OrL" -> pure $ arithmeticNode OrL nodeId rEdges
    -- Left shifting
    "LShiftI" -> pure $ arithmeticNode LShiftI nodeId rEdges
    "LShiftL" -> pure $ arithmeticNode LShiftL nodeId rEdges
    -- Right shifting
    "RShiftI" -> pure $ arithmeticNode RShiftI nodeId rEdges
    "RShiftL" -> pure $ arithmeticNode RShiftL nodeId rEdges
    -- Conversions
    "ConvD2F" -> pure $ arithmeticNode ConvD2F nodeId rEdges
    "ConvD2I" -> pure $ arithmeticNode ConvD2I nodeId rEdges
    "ConvD2L" -> pure $ arithmeticNode ConvD2L nodeId rEdges
    "ConvF2D" -> pure $ arithmeticNode ConvF2D nodeId rEdges
    "ConvF2I" -> pure $ arithmeticNode ConvF2I nodeId rEdges
    "ConvF2L" -> pure $ arithmeticNode ConvF2L nodeId rEdges
    "ConvI2D" -> pure $ arithmeticNode ConvI2D nodeId rEdges
    "ConvI2F" -> pure $ arithmeticNode ConvI2F nodeId rEdges
    "ConvI2L" -> pure $ arithmeticNode ConvI2L nodeId rEdges
    "ConvL2D" -> pure $ arithmeticNode ConvL2D nodeId rEdges
    "ConvL2F" -> pure $ arithmeticNode ConvL2F nodeId rEdges
    "ConvL2I" -> pure $ arithmeticNode ConvL2I nodeId rEdges
    -- Comparisons
    "CmpI" -> pure $ arithmeticNode CmpI nodeId rEdges
    "CmpL" -> pure $ arithmeticNode CmpL nodeId rEdges
    "CmpF" -> pure $ arithmeticNode CmpF nodeId rEdges
    "CmpD" -> pure $ arithmeticNode CmpD nodeId rEdges
    "Bool" ->
      let mkBool boolOp = case singlePred nodeId rEdges of
            Right pred -> Parsed (nodeId, Bool boolOp pred)
            Left err -> Unsupported $ "Bool: " <> err
       in case M.lookup "dump_spec" nodeProps of
            Just "[ne]" -> pure $ mkBool Ne
            Just other -> pure $ Unsupported $ "Bool: Unrecognised dump_spec: " <> other
            Nothing -> pure $ Unsupported "Bool: Missing dump_spec"
    "Phi" ->
      -- NOTE: drop 1 removes the first predecessor, which is guaratneed by SoN to be a region node
      -- For Phi nodes, we only care for dataflow predecessors
      case (drop 1 $ findNodePred nodeId rEdges) of
        preds | length preds > 1 -> pure $ Parsed (nodeId, Phi nodeId preds)
        _ -> pure $ Unsupported "Phi: Phi node got less than two predecessors"
    "Region" ->
      case (findNodePred nodeId rEdges) of
        preds | length preds > 1 -> pure $ Parsed (nodeId, Region nodeId preds)
        _ -> pure $ Unsupported "Region: Region node got less than two predecessors"
    "If" ->
      case (findNodePred nodeId rEdges) of
        [ctrl, boolExpr] -> pure $ Parsed (nodeId, If nodeId boolExpr)
        preds -> pure $ Unsupported $ "If: Expected two predecessors but got " <> show (length preds)
    "IfTrue" -> pure $ Parsed (nodeId, IfTrue nodeId)
    "IfFalse" -> pure $ Parsed (nodeId, IfFalse nodeId)
    "Return" ->
      case (findNodePred nodeId rEdges) of
        [ctrl, _parm6, _mem, _rawptr, _retAddress, dataPred] -> pure $ Parsed (nodeId, Return dataPred)
        preds -> pure $ Unsupported $ "Return: Expected 6 predecessors but got " <> show (length preds)
    _ -> pure $ Ignored

-- | Finds the single predecessor of a node.
-- Fails if it finds more than one.
singlePred :: NodeId -> [RawEdge] -> Either String NodeId
singlePred nodeId edges = case findNodePred nodeId edges of
  [pred] -> Right pred
  preds -> Left $ "Expected one predecessor but got " <> show (length preds)

-- | Given the data constructor of an arithmetic node, find the node id of its predecessors to create
-- the corresponding @Node@.
arithmeticNode :: (NodeId -> NodeId -> Node) -> NodeId -> [(NodeId, NodeId, NodeId)] -> ParseResult (NodeId, Node)
arithmeticNode constr nodeId rEdges =
  case findNodePred nodeId rEdges of
    [x, y] -> Parsed (nodeId, constr x y)
    neighbors -> Unsupported $ "Arithmetic node: Expected two preds but got: " <> show (length neighbors)

-- | Given a NodeId and a list of raw edges, find the predecessors of the node
-- and return them in an _ordered_ list.
-- e.g. Given nodeId = 1 and raw edges [(3, 1, 1), (2, 1, 0)]
--      findNodePred -> [2, 3]
findNodePred :: NodeId -> [RawEdge] -> [NodeId]
findNodePred = go []
  where
    go acc _ [] = fst <$> sortOn snd acc
    go acc nid ((from, to, idx) : rest) =
      if nid == to
        then go ((from, idx) : acc) nid rest
        else go acc nid rest

-- | Similar to @findNodePred@, except it finds the successors
findNodeSucc :: NodeId -> [RawEdge] -> [NodeId]
findNodeSucc = go []
  where
    go acc _ [] = fst <$> sortOn snd acc
    go acc nid ((from, to, idx) : rest) =
      if nid == from
        then go ((to, idx) : acc) nid rest
        else go acc nid rest

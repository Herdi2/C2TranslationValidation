{-# LANGUAGE TupleSections #-}

module GraphBuilder (buildGraph) where

import Control.Monad (unless)
import Control.Monad.State.Lazy
import Data.List (sortOn)
import qualified Data.Map as M
import Debug.Trace
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

buildGraph :: RawGraph -> Either String Graph
buildGraph (RawGraph rNodes rEdges) =
  do
    let (unsupported, parsedNodes) = partitionParseResults $ (buildNode rEdges) <$> rNodes
    unless (null unsupported) (Left $ unlines unsupported)
    controlFlow <- buildCtrlflow rEdges parsedNodes
    Right $
      defaultGraph
        { nodeInfo = M.fromList parsedNodes,
          controlSuccessors = controlFlow
        }

buildNode ::
  [(NodeId, NodeId, NodeId)] ->
  RawNode ->
  ParseResult (NodeId, Node)
buildNode rEdges (RawNode (read -> nodeId) nodeName nodeProps) =
  case nodeName of
    "Start" -> Ignored
    "Root" -> Ignored
    "Con" -> Ignored
    "Parm" ->
      case (M.lookup "type" nodeProps) of
        Just "int:" -> Parsed (nodeId, ParmI nodeId)
        Just "long:" -> Parsed (nodeId, ParmL nodeId)
        Just "float:" -> Parsed (nodeId, ParmF nodeId)
        Just "double:" -> Parsed (nodeId, ParmD nodeId)
        Just "control" -> Parsed (nodeId, ParmCtrl nodeId)
        Just "abIO" -> Ignored
        Just "rawptr:" -> Ignored
        Just "return_address" -> Ignored
        Just "memory" -> Ignored
        Just t -> Unsupported $ "Unsupported param of type: " <> t
        Nothing -> Unsupported $ "buildNode: Internal error, node property didn't contain \"type\""
    -- Constants
    "ConI" -> Parsed (nodeId, ConI $ read (drop (length "int:") $ nodeProps M.! "bottom_type"))
    "ConL" -> Parsed (nodeId, ConL $ read (drop (length "long:") $ nodeProps M.! "short_name"))
    -- NOTE: Float and Double is assumed to be given in binary format
    "ConF" -> Parsed (nodeId, ConF $ fromInteger $ read (drop (length "float:") $ nodeProps M.! "short_name"))
    "ConD" -> Parsed (nodeId, ConD $ fromInteger $ read (drop (length "double:") $ nodeProps M.! "short_name"))
    -- Addition
    "AddI" -> arithmeticNode AddI nodeId rEdges
    "AddL" -> arithmeticNode AddL nodeId rEdges
    "AddF" -> arithmeticNode AddF nodeId rEdges
    "AddD" -> arithmeticNode AddD nodeId rEdges
    -- Subtraction
    "SubI" -> arithmeticNode SubI nodeId rEdges
    "SubL" -> arithmeticNode SubL nodeId rEdges
    "SubF" -> arithmeticNode SubF nodeId rEdges
    "SubD" -> arithmeticNode SubD nodeId rEdges
    -- Multiplication
    "MulI" -> arithmeticNode MulI nodeId rEdges
    "MulL" -> arithmeticNode MulL nodeId rEdges
    "MulF" -> arithmeticNode MulF nodeId rEdges
    "MulD" -> arithmeticNode MulD nodeId rEdges
    "MulHiL" -> arithmeticNode MulHiL nodeId rEdges
    -- Division
    "DivI" -> arithmeticNode DivI nodeId rEdges
    "DivL" -> arithmeticNode DivL nodeId rEdges
    "DivF" -> arithmeticNode DivF nodeId rEdges
    "DivD" -> arithmeticNode DivD nodeId rEdges
    -- Bitwise and
    "AndI" -> arithmeticNode AndI nodeId rEdges
    "AndL" -> arithmeticNode AndL nodeId rEdges
    -- Bitwise or
    "OrI" -> arithmeticNode OrI nodeId rEdges
    "OrL" -> arithmeticNode OrL nodeId rEdges
    -- Left shifting
    "LShiftI" -> arithmeticNode LShiftI nodeId rEdges
    "LShiftL" -> arithmeticNode LShiftL nodeId rEdges
    -- Right shifting
    "RShiftI" -> arithmeticNode RShiftI nodeId rEdges
    "RShiftL" -> arithmeticNode RShiftL nodeId rEdges
    -- Conversions
    "ConvD2F" -> arithmeticNode ConvD2F nodeId rEdges
    "ConvD2I" -> arithmeticNode ConvD2I nodeId rEdges
    "ConvD2L" -> arithmeticNode ConvD2L nodeId rEdges
    "ConvF2D" -> arithmeticNode ConvF2D nodeId rEdges
    "ConvF2I" -> arithmeticNode ConvF2I nodeId rEdges
    "ConvF2L" -> arithmeticNode ConvF2L nodeId rEdges
    "ConvI2D" -> arithmeticNode ConvI2D nodeId rEdges
    "ConvI2F" -> arithmeticNode ConvI2F nodeId rEdges
    "ConvI2L" -> arithmeticNode ConvI2L nodeId rEdges
    "ConvL2D" -> arithmeticNode ConvL2D nodeId rEdges
    "ConvL2F" -> arithmeticNode ConvL2F nodeId rEdges
    "ConvL2I" -> arithmeticNode ConvL2I nodeId rEdges
    -- Comparisons
    "CmpI" -> arithmeticNode CmpI nodeId rEdges
    "CmpL" -> arithmeticNode CmpL nodeId rEdges
    "CmpF" -> arithmeticNode CmpF nodeId rEdges
    "CmpD" -> arithmeticNode CmpD nodeId rEdges
    "Bool" ->
      let mkBool boolOp = case singlePred nodeId rEdges of
            Right pred -> Parsed (nodeId, Bool boolOp pred)
            Left err -> Unsupported $ "Bool: " <> err
       in case M.lookup "dump_spec" nodeProps of
            Just "[ne]" -> mkBool Ne
            Just other -> Unsupported $ "Bool: Unrecognised dump_spec: " <> other
            Nothing -> Unsupported "Bool: Missing dump_spec"
    "Phi" ->
      case (findNodePred nodeId rEdges) of
        (regionId : preds) | length preds > 1 -> Parsed (nodeId, Phi regionId preds)
        preds -> Unsupported $ "Phi: Phi node got less than two predecessors"
    "Region" ->
      -- NOTE: A region always has an edge to itself, which is the first predecessor
      case (findNodePred nodeId rEdges) of
        (regionId : preds) | length preds > 1 -> Parsed (nodeId, Region nodeId preds)
        _ -> Unsupported "Region: Region node got less than two predecessors"
    "If" ->
      case (findNodePred nodeId rEdges) of
        -- "After Parsing" has two predecessors, ctrl flow and boolean expr.
        -- "Before Matching" adds a predecessor, the arithmetic expr input to the boolean node
        [_ctrl, boolExpr] -> Parsed (nodeId, If nodeId boolExpr)
        [_ctrl, boolExpr, _expr] -> Parsed (nodeId, If nodeId boolExpr)
        preds -> Unsupported $ "If: Expected two or three predecessors but got " <> show (length preds)
    "IfTrue" -> Parsed (nodeId, IfTrue nodeId)
    "IfFalse" -> Parsed (nodeId, IfFalse nodeId)
    "Return" ->
      -- NOTE: Our verification condition is built on having a return value,
      -- so return always has a dataflow predecessor.
      case (findNodePred nodeId rEdges) of
        [_ctrl, _parm6, _mem, _rawptr, _retAddress, dataPred] -> Parsed (nodeId, Return nodeId dataPred)
        preds -> Unsupported $ "Return: Expected 6 predecessors but got " <> show (length preds)
    _ -> Unsupported $ "buildNode: Unsupported node with name " <> nodeName

-- | Given a control flow node, returns the predecessors of the node
buildCtrlflow :: [RawEdge] -> [(NodeId, Node)] -> Either String ControlSuccessors
buildCtrlflow rEdges nodes =
  do
    let (unsupported, ctrlEdges) = partitionParseResults $ (handleNode rEdges) <$> nodes
    unless (null unsupported) (Left $ unlines unsupported)
    Right $ M.fromListWith (++) $ ctrlEdges >>= id
  where
    handleNode rEdges node =
      case snd node of
        ParmCtrl nid -> Ignored
        Region nid preds -> Parsed $ (,[nid]) <$> preds
        If nid _boolExpr ->
          -- NOTE: The order is very important for correctly assigning if-else
          case findNodePred nid rEdges of
            [ctrlPred, _boolExpr] -> Parsed [(ctrlPred, [nid])]
            [ctrlPred, boolExpr, _expr] -> Parsed [(ctrlPred, [nid])]
            succs -> Unsupported $ "If: Expected two predecessors but got " <> show (length succs)
        IfFalse nid ->
          case findNodePred nid rEdges of
            [ctrlPred] -> Parsed [(ctrlPred, [nid])]
            succs -> Unsupported $ "ParmCtrl: Expected one predecessor but got " <> show (length succs)
        IfTrue nid ->
          case findNodePred nid rEdges of
            [ctrlPred] -> Parsed [(ctrlPred, [nid])]
            succs -> Unsupported $ "ParmCtrl: Expected one successor but got " <> show (length succs)
        -- A return does not have any successors
        Return nid _ ->
          case (findNodePred nid rEdges) of
            [ctrlPred, _parm6, _mem, _rawptr, _retAddress, _dataPred] -> Parsed [(ctrlPred, [nid])]
            preds -> Unsupported $ "Return: Expected 6 predecessors but got " <> show (length preds)
        -- Any node not handled is assumed to not be part of the control flow, and is safely ignored
        _ -> Ignored

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

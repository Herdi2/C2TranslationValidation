{-# LANGUAGE TupleSections #-}

module Verifier.GraphBuilder (buildGraph) where

import Control.Monad (unless)
import Control.Monad.State.Lazy
import Data.Char (isDigit)
import Data.List (isPrefixOf, sortOn)
import qualified Data.Map as M
import Debug.Trace
import Verifier.Graph
import Text.Read (readMaybe)

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
      let matchType typ
            | "int" `isPrefixOf` typ = Parsed (nodeId, ParmI nodeId)
            | "long" `isPrefixOf` typ = Parsed (nodeId, ParmL nodeId)
            | "float" `isPrefixOf` typ = Parsed (nodeId, ParmF nodeId)
            | "double" `isPrefixOf` typ = Parsed (nodeId, ParmD nodeId)
            | "control" `isPrefixOf` typ = Parsed (nodeId, ParmCtrl nodeId)
            | "abIO" `isPrefixOf` typ = Ignored
            | "rawptr" `isPrefixOf` typ = Ignored
            | "return_address" `isPrefixOf` typ = Ignored
            | "memory" `isPrefixOf` typ = Ignored
            | otherwise = Unsupported $ "Unsupported param of type: " <> typ
       in case (M.lookup "type" nodeProps) of
            Nothing -> Unsupported $ "buildNode: Internal error, node property didn't contain \"type\""
            Just typ -> matchType typ
    -- Constants
    "ConI" ->
      case (M.lookup "bottom_type" nodeProps) of
        Nothing -> Unsupported $ "buildNode: Internal error, ConI didn't contain \"bottom_type\""
        Just value ->
          case (readMaybe (drop (length "int:") value)) of
            Nothing -> Unsupported $ "buildNode: Internal error, ConI, couldn't read value from " <> show value
            Just readRes -> Parsed (nodeId, ConI readRes)
    "ConL" ->
      case (M.lookup "bottom_type" nodeProps) of
        Nothing -> Unsupported $ "buildNode: Internal error, ConL didn't contain \"bottom_type\""
        Just value ->
          case (readMaybe (drop (length "long:") value)) of
            Nothing -> Unsupported $ "buildNode: Internal error, ConI, couldn't read value from " <> show value
            Just readRes -> Parsed (nodeId, ConL readRes)
    -- NOTE: ConF and ConD are assumed to be given in a binary format
    "ConF" ->
      case (M.lookup "bottom_type" nodeProps) of
        Nothing -> Unsupported $ "buildNode: Internal error, ConF didn't contain \"bottom_type\""
        Just value ->
          case (readMaybe (drop (length "ftcon:") value)) of
            Nothing -> Unsupported $ "buildNode: Internal error, ConF, couldn't read value from " <> show value
            Just readRes -> Parsed (nodeId, ConF $ fromInteger readRes)
    "ConD" ->
      case (M.lookup "bottom_type" nodeProps) of
        Nothing -> Unsupported $ "buildNode: Internal error, ConD didn't contain \"bottom_type\""
        Just value ->
          case (readMaybe (drop (length "dblcon:") value)) of
            Nothing -> Unsupported $ "buildNode: Internal error, ConD, couldn't read value from " <> show value
            Just readRes -> Parsed (nodeId, ConD $ fromInteger readRes)
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
    "ConvD2F" -> convNode ConvD2F nodeId rEdges
    "ConvD2I" -> convNode ConvD2I nodeId rEdges
    "ConvD2L" -> convNode ConvD2L nodeId rEdges
    "ConvF2D" -> convNode ConvF2D nodeId rEdges
    "ConvF2I" -> convNode ConvF2I nodeId rEdges
    "ConvF2L" -> convNode ConvF2L nodeId rEdges
    "ConvI2D" -> convNode ConvI2D nodeId rEdges
    "ConvI2F" -> convNode ConvI2F nodeId rEdges
    "ConvI2L" -> convNode ConvI2L nodeId rEdges
    "ConvL2D" -> convNode ConvL2D nodeId rEdges
    "ConvL2F" -> convNode ConvL2F nodeId rEdges
    "ConvL2I" -> convNode ConvL2I nodeId rEdges
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


convNode :: (NodeId -> Node) -> NodeId -> [(NodeId, NodeId, NodeId)] -> ParseResult (NodeId, Node)
convNode constr nodeId rEdges =
  case findNodePred nodeId rEdges of
    [x] -> Parsed (nodeId, constr x)
    neighbors -> Unsupported $ "Conversion node: Expected one pred but got: " <> show (length neighbors)

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

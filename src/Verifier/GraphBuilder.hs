{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Verifier.GraphBuilder (buildGraph, readPtrType) where

import Control.Monad (unless)
import Data.Char (isDigit)
import Data.List (find, isPrefixOf, sortOn)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.SBV (SBool, free_, fromBool)
import Debug.Trace
import GHC.Float (castWord32ToFloat, castWord64ToDouble)
import Numeric (readBin)
import Text.Read (readMaybe)
import Verifier.Graph

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
buildGraph rawGraph@(RawGraph rNodes rEdges) =
  do
    let (unsupported, parsedNodes) = partitionParseResults $ (buildNode rawGraph) <$> rNodes
    unless (null unsupported) (Left $ unlines unsupported)
    controlFlow <- buildCtrlflow rEdges parsedNodes
    retType <- findMethodType rawGraph
    Right $
      defaultGraph
        { methodType = retType,
          nodeInfo = M.fromList parsedNodes,
          controlSuccessors = controlFlow
        }

-- | Needed to keep track of method return type, to allow for mergeable tuples with sideeffects
-- See @Ret@.
-- NOTE: Yes, I was overly cautious writing this method, I like error messages.
findMethodType ::
  RawGraph ->
  Either String JType
findMethodType (RawGraph rNodes rEdges) =
  do
    (RawNode retNid _ _) <-
      fromMaybe
        (Left $ "findMethodType: Did not find return statement")
        (Right <$> find (\(RawNode _ nodeName _) -> nodeName == "Return") rNodes)
    dataNid <-
      case (findNodePred (read retNid) rEdges) of
        [_ctrl, _parm6, _mem, _rawptr, _retAddress, dataPred] -> Right dataPred
        preds -> Left $ "Return: Expected 6 predecessors but got " <> show (length preds)
    (RawNode _ _ dataProps) <-
      fromMaybe
        (Left $ "findMethodType: Could not find data pred with id " <> show dataNid)
        (Right <$> find (\(RawNode nid _ _) -> read nid == dataNid) rNodes)
    let botType = M.lookup "bottom_type" dataProps
    case botType of
      Just typ
        | "int" `isPrefixOf` typ -> Right JINT
        | "long" `isPrefixOf` typ -> Right JLONG
        | "float" `isPrefixOf` typ -> Right JFLOAT
        | "double" `isPrefixOf` typ -> Right JDOUBLE
        | "fltcon" `isPrefixOf` typ -> Right JFLOAT
        | "dblcon" `isPrefixOf` typ -> Right JDOUBLE
        | otherwise -> Left $ "findMethodType: Unsupported return type " <> typ
      Nothing -> Left $ "findMethodType: Data node had no \"bottom_type\""

buildNode ::
  RawGraph ->
  RawNode ->
  ParseResult (NodeId, Node)
buildNode (RawGraph rNodes rEdges) (RawNode (read -> nodeId) nodeName nodeProps) =
  case nodeName of
    "Start" -> Ignored
    "Root" -> Ignored
    "Con" -> Ignored
    -- Proj and Halt mainly used for side effects (CallStatic)
    "Proj" -> Ignored
    "Halt" -> Ignored
    "CallStaticJava" -> Parsed (nodeId, CallStatic nodeId)
    "Parm" ->
      let matchType typ
            | "int" `isPrefixOf` typ = Parsed (nodeId, ParmI nodeId)
            | "long" `isPrefixOf` typ = Parsed (nodeId, ParmL nodeId)
            | "float" `isPrefixOf` typ = Parsed (nodeId, ParmF nodeId)
            | "double" `isPrefixOf` typ = Parsed (nodeId, ParmD nodeId)
            | "memory" `isPrefixOf` typ = Parsed (nodeId, ParmMem)
            | "inst" `isPrefixOf` typ =
                case (readPtrType <$> (M.lookup "bottom_type" nodeProps)) of
                  Nothing ->
                    Unsupported $
                      "Pointer param: Internal error, node property didn't contain \"bottom_type\" or invalid ptr string given"
                  Just (Left err) -> Unsupported $ "Pointer param: " <> err
                  Just (Right memIndex) -> Parsed (nodeId, ParmMemPtr nodeId memIndex)
            | "control" `isPrefixOf` typ = Parsed (nodeId, ParmCtrl nodeId)
            | "abIO" `isPrefixOf` typ = Ignored
            | "rawptr" `isPrefixOf` typ = Ignored
            | "return_address" `isPrefixOf` typ = Ignored
            | otherwise = Unsupported $ "Unsupported param of type: " <> typ
       in case (M.lookup "type" nodeProps) of
            -- NOTE: Using "type" instead of "bottom_type" since control flow param does not have bottom_type
            Nothing -> Unsupported $ "buildNode: Internal error, node " <> nodeName <> " property didn't contain \"type\""
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
    -- NOTE: Assuming floats and doubles are given in binary format,
    -- to avoid floating-point precision loss.
    "ConF" ->
      case (M.lookup "bottom_type" nodeProps) of
        Nothing -> Unsupported $ "buildNode: Internal error, ConF didn't contain \"bottom_type\""
        Just value ->
          let floatBin = drop (length "ftcon:") value
              binDigit x = x == '0' || x == '1'
           in if all binDigit floatBin
                then Parsed (nodeId, ConF $ bitsToFloat floatBin)
                else Unsupported $ "Floating point expected in binary, but got " <> floatBin
      where
        bitsToFloat = castWord32ToFloat . fst . head . readBin
    "ConD" ->
      case (M.lookup "bottom_type" nodeProps) of
        Nothing -> Unsupported $ "buildNode: Internal error, ConD didn't contain \"bottom_type\""
        Just value ->
          let doubleBin = drop (length "dblcon:") value
              binDigit x = x == '0' || x == '1'
           in if all binDigit doubleBin
                then Parsed (nodeId, ConD $ bitsToDouble doubleBin)
                else Unsupported $ "Double expected in binary, but got " <> doubleBin
      where
        bitsToDouble = castWord64ToDouble . fst . head . readBin
    "ConP" ->
      case (readPtrType <$> (M.lookup "bottom_type" nodeProps)) of
        Nothing ->
          Unsupported $ "Pointer param: Internal error, node property didn't contain \"bottom_type\" or invalid ptr string given"
        Just (Left err) -> Unsupported $ "ConP: " <> err
        Just (Right memIndex) -> Parsed $ (nodeId, ConP memIndex)
    -- Addition
    "AddI" -> arithmeticNode "AddI" AddI nodeId rEdges
    "AddL" -> arithmeticNode "AddL" AddL nodeId rEdges
    "AddF" -> arithmeticNode "AddF" AddF nodeId rEdges
    "AddD" -> arithmeticNode "AddD" AddD nodeId rEdges
    -- Subtraction
    "SubI" -> arithmeticNode "SubI" SubI nodeId rEdges
    "SubL" -> arithmeticNode "SubL" SubL nodeId rEdges
    "SubF" -> arithmeticNode "SubF" SubF nodeId rEdges
    "SubD" -> arithmeticNode "SubD" SubD nodeId rEdges
    -- Multiplication
    "MulI" -> arithmeticNode "MulI" MulI nodeId rEdges
    "MulL" -> arithmeticNode "MulL" MulL nodeId rEdges
    "MulF" -> arithmeticNode "MulF" MulF nodeId rEdges
    "MulD" -> arithmeticNode "MulD" MulD nodeId rEdges
    "MulHiL" -> arithmeticNode "MulHiL" MulHiL nodeId rEdges
    -- Division
    "DivI" -> pinnedArithmeticNode "DivI" DivI nodeId rEdges
    "DivL" -> pinnedArithmeticNode "DivL" DivL nodeId rEdges
    "DivF" -> pinnedArithmeticNode "DivF" DivF nodeId rEdges
    "DivD" -> pinnedArithmeticNode "DivD" DivD nodeId rEdges
    -- Bitwise and
    "AndI" -> arithmeticNode "AndI" AndI nodeId rEdges
    "AndL" -> arithmeticNode "AndL" AndL nodeId rEdges
    -- Bitwise or
    "OrI" -> arithmeticNode "OrI" OrI nodeId rEdges
    "OrL" -> arithmeticNode "OrL" OrL nodeId rEdges
    -- Left shifting
    "LShiftI" -> arithmeticNode "LShiftI" LShiftI nodeId rEdges
    "LShiftL" -> arithmeticNode "LShiftL" LShiftL nodeId rEdges
    -- Right shifting
    "RShiftI" -> arithmeticNode "RShiftI" RShiftI nodeId rEdges
    "RShiftL" -> arithmeticNode "RShiftL" RShiftL nodeId rEdges
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
    "CmpI" -> arithmeticNode "CmpI" CmpI nodeId rEdges
    "CmpL" -> arithmeticNode "CmpL" CmpL nodeId rEdges
    "CmpF" -> arithmeticNode "CmpF" CmpF nodeId rEdges
    "CmpD" -> arithmeticNode "CmpD" CmpD nodeId rEdges
    "Bool" ->
      let mkBool boolOp = case singlePred nodeId rEdges of
            Right pred -> Parsed (nodeId, Bool boolOp pred)
            Left err -> Unsupported $ "Bool: " <> err
       in case M.lookup "dump_spec" nodeProps of
            Just "[ne]" -> mkBool Ne
            Just "[le]" -> mkBool Le
            Just "[lt]" -> mkBool Lt
            Just "[eq]" -> mkBool Ee
            Just other -> Unsupported $ "Bool: Unrecognised dump_spec: " <> other
            Nothing -> Unsupported "Bool: Missing dump_spec"
    "Phi" ->
      case (findNodePred nodeId rEdges) of
        (regionId : preds) | length preds > 1 -> Parsed (nodeId, Phi regionId preds)
        preds -> Unsupported $ "Phi: Phi node got less than two predecessors"
    -- \| CONTROL FLOW
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
    -- \| MEMORY FLOW
    "CastPP" -> Ignored
    "CmpP" -> arithmeticNode "CmpP" CmpP nodeId rEdges
    "StoreI" -> storeNode (M.lookup "dump_spec" nodeProps) StoreI nodeId rEdges
    "StoreL" -> storeNode (M.lookup "dump_spec" nodeProps) StoreL nodeId rEdges
    "StoreF" -> storeNode (M.lookup "dump_spec" nodeProps) StoreF nodeId rEdges
    "StoreD" -> storeNode (M.lookup "dump_spec" nodeProps) StoreD nodeId rEdges
    "StoreP" -> storeNode (M.lookup "dump_spec" nodeProps) StoreP nodeId rEdges
    "LoadI" -> loadNode "LoadI" LoadI (M.lookup "source" nodeProps) nodeId rEdges
    "LoadL" -> loadNode "LoadL" LoadL (M.lookup "source" nodeProps) nodeId rEdges
    "LoadF" -> loadNode "LoadF" LoadF (M.lookup "source" nodeProps) nodeId rEdges
    "LoadD" -> loadNode "LoadD" LoadD (M.lookup "source" nodeProps) nodeId rEdges
    "LoadP" ->
      case (readPtrType <$> M.lookup "bottom_type" nodeProps, M.lookup "source" nodeProps) of
        (Nothing, _) -> Unsupported "LoadP: Didn't contain \"bottom_type\""
        (_, Nothing) -> Unsupported "LoadP: Didn't contain \"source\""
        (Just (Left err), _) -> Unsupported $ "LoadP: " <> err
        (Just (Right ptrType), Just source) ->
          case findNodePred nodeId rEdges of
            [x, y] -> Parsed (nodeId, LoadP source ptrType x y)
            [_ctrl, x, y] -> Parsed (nodeId, LoadP source ptrType x y)
            neighbors -> Unsupported $ nodeName <> ": Expected two preds but got: " <> show (length neighbors)
    "AddP" ->
      -- NOTE: Here we shortcut. The final value calculated by AddP is always* given
      -- in the "dump_spec", and as such we can immediately store it in the AddP node.
      -- \*atleast from observation
      case (findNodePred nodeId rEdges) of
        [ptr1, ptr2, val] ->
          case (readPtrType <$> M.lookup "bottom_type" nodeProps) of
            Nothing -> Unsupported $ "AddP: No attribute \"bottom_type\""
            Just (Left err) -> Unsupported $ "AddP: " <> show err
            Just (Right memIndex) ->
              Parsed $ (nodeId, AddP memIndex ptr1 ptr2 val)
        preds -> Unsupported $ "AddP: Expected three predecessors but got " <> show (length preds)
    "MergeMem" ->
      case (findNodePred nodeId rEdges) of
        (_top1 : parmMem : rest) -> Parsed (nodeId, MergeMem parmMem (filter (not . isTop) rest))
        preds -> Unsupported $ "MergeMem: Expected at least two predecessors but got " <> show (length preds)
      where
        isTop nid =
          case (find ((==) nid . read . rawNodeId) rNodes) of
            Just n -> (==) "Con" $ rawNodeName n
            Nothing -> True
    _ -> Unsupported $ "buildNode: Unsupported node  (" <> show nodeId <> ", " <> nodeName <> ")"

-- | Given the data constructor of a store node, find the node id of its predecessors to create
-- the corresponding @Node@.
storeNode ::
  Maybe String ->
  (MemLattice -> NodeId -> NodeId -> NodeId -> Node) ->
  NodeId ->
  [(NodeId, NodeId, NodeId)] ->
  ParseResult (NodeId, Node)
storeNode Nothing _ _ _ = Unsupported "Store node: No attribute named \"dump_spec\". Needed to determine slice."
storeNode (Just attr) constr nodeId rEdges =
  case findNodePred nodeId rEdges of
    [_ctrl, mem, addr, val] ->
      case findMemIdx attr of
        Just slice -> Parsed (nodeId, constr slice mem addr val)
        Nothing -> Unsupported $ "Store node: Could not find mem idx from props: " <> show attr
    neighbors -> Unsupported $ "Store node: Expected four preds but got: " <> show (length neighbors)
  where
    -- \| In dump_spec, the slice is given in the attribute `idx=<slice>`
    findMemIdx [] = Nothing
    findMemIdx ('i' : 'd' : 'x' : '=' : slice)
      | "Bot" `isPrefixOf` slice = Just Bot
      | otherwise = Slice <$> readMaybe (takeWhile isDigit slice)
    findMemIdx (_ : rest) = findMemIdx rest

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
        CallStatic nid ->
          case (findNodePred nid rEdges) of
            (ctrlPred : rest) -> Parsed [(ctrlPred, [nid])]
            preds -> Unsupported "CallStatic: got zero predecessors"
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
arithmeticNode :: String -> (NodeId -> NodeId -> Node) -> NodeId -> [(NodeId, NodeId, NodeId)] -> ParseResult (NodeId, Node)
arithmeticNode nodeName constr nodeId rEdges =
  case findNodePred nodeId rEdges of
    [x, y] -> Parsed (nodeId, constr x y)
    neighbors -> Unsupported $ nodeName <> ": Expected two preds but got: " <> show (length neighbors)

-- | Similar to @arithmeticNode@, except it allows for the node to get pinned by control flow.
-- This can happen for division.
pinnedArithmeticNode :: String -> (NodeId -> NodeId -> Node) -> NodeId -> [(NodeId, NodeId, NodeId)] -> ParseResult (NodeId, Node)
pinnedArithmeticNode nodeName constr nodeId rEdges =
  -- \| NOTE: Unlike other arithmetic nodes, Div can get pinned by control flow
  case findNodePred nodeId rEdges of
    [x, y] -> Parsed (nodeId, constr x y)
    [_ctrl, x, y] -> Parsed (nodeId, constr x y)
    neighbors -> Unsupported $ nodeName <> ": Expected two preds but got: " <> show (length neighbors)

-- | Similar to @arithmeticNode@, except it allows for the node to get pinned by control flow.
-- This can happen for division.
loadNode :: String -> (String -> NodeId -> NodeId -> Node) -> Maybe String -> NodeId -> [(NodeId, NodeId, NodeId)] -> ParseResult (NodeId, Node)
loadNode nodeName _ Nothing _ _ = Unsupported $ nodeName <> ": did not contain attribute \"source\""
loadNode nodeName constr (Just src) nodeId rEdges =
  -- \| NOTE: Unlike other arithmetic nodes, Div can get pinned by control flow
  case findNodePred nodeId rEdges of
    [x, y] -> Parsed (nodeId, constr src x y)
    [_ctrl, x, y] -> Parsed (nodeId, constr src x y)
    neighbors -> Unsupported $ nodeName <> ": Expected two preds but got: " <> show (length neighbors)

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

-- | Read the pointer type of a pointer node
-- Structure of a pointer: <refinement>:<className>:<ObjectStatus>+<offset>,iid=<something>
-- e.g. instptr:Object3:NotNull+16,iid=bot
-- becomes: JPointer ("Object3", 16) InstPtr NotNull Nothing
-- The `Nothing` is placeholder for a symbolic boolean,
-- which indicates whether the pointer is a nullptr or not.
-- It will be filled during verification.
-- NOTE: The iid (instance id) is ignored for now
readPtrType :: String -> Either String SValue
readPtrType (takeWhile (/= ',') -> ptrStr) =
  -- NOTE: Here we remove iid
  case parseAttr ptrStr of
    ("instptr", rest) ->
      let (className, rest') = parseAttr rest
          (objStatus, '+' : offset) = parseAttr rest'
       in Right $ JPointer (className, read offset) InstPtr (parseObjStatus objStatus) Nothing
    ("ptr", rest) ->
      case (parseAttr rest) of
        ("null", _) -> Right NullPtr
        (e, _) -> Left $ "readPtrType: Unrecognized ptr value " <> e
    (p, _) -> Left $ "Unrecognized pointer type: " <> p
  where
    parseAttr :: String -> (String, String)
    parseAttr a =
      let (attribute, rest) = span (\x -> x /= ':' && x /= '+') a
       in case rest of
            (':' : r) -> (attribute, r)
            r -> (attribute, r)
    parseObjStatus =
      \case
        "NotNull" -> NotNull
        "Null" -> Null
        "BotPTR" -> BotPTR

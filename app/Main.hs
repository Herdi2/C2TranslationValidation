{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Main (main) where

import Control.Monad
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.Proxy
import Data.SBV
import Debug.Trace
import GHC.Generics
import GHC.Natural

type NodeId = Word32

type ParamMap = M.Map NodeId SValue

type NodeInfo = M.Map NodeId Node

data JKind
  = IntKind
  | LongKind
  | FloatKind
  | DoubleKind
  deriving (Show, Eq)

data Comp
  = Ne
  deriving (Show, Eq)

data Node
  = -- Parameters carry their own nodeId for parameter handling (see @createParams@)
    ParmI NodeId
  | ParmL NodeId
  | ParmF NodeId
  | ParmD NodeId
  | -- | Constant nodes
    ConI Int32
  | ConL Int64
  | ConF FPSingle
  | ConD FPDouble
  | -- | Addition
    AddI NodeId NodeId
  | AddL NodeId NodeId
  | AddF NodeId NodeId
  | AddD NodeId NodeId
  | -- | Subtraction
    SubI NodeId NodeId
  | SubL NodeId NodeId
  | SubF NodeId NodeId
  | SubD NodeId NodeId
  | -- | Multiplication
    MulI NodeId NodeId
  | MulL NodeId NodeId
  | MulF NodeId NodeId
  | MulD NodeId NodeId
  | -- | Special mult between longs
    MulHiL NodeId NodeId
  | -- | Division
    DivI NodeId NodeId
  | DivL NodeId NodeId
  | DivF NodeId NodeId
  | DivD NodeId NodeId
  | -- | Bitwise and
    AndI NodeId NodeId
  | AndL NodeId NodeId
  | -- | Bitwise or
    OrI NodeId NodeId
  | OrL NodeId NodeId
  | -- | Left shifting
    LShiftI NodeId NodeId
  | LShiftL NodeId NodeId
  | -- | Right shifting
    RShiftI NodeId NodeId
  | RShiftL NodeId NodeId
  | -- | Conversion between int, long, float, double
    ConvD2F NodeId NodeId
  | ConvD2I NodeId NodeId
  | ConvD2L NodeId NodeId
  | ConvF2D NodeId NodeId
  | ConvF2I NodeId NodeId
  | ConvF2L NodeId NodeId
  | ConvI2D NodeId NodeId
  | ConvI2F NodeId NodeId
  | ConvI2L NodeId NodeId
  | ConvL2D NodeId NodeId
  | ConvL2F NodeId NodeId
  | ConvL2I NodeId NodeId
  | -- | Comparisons
    CmpI NodeId NodeId
  | CmpL NodeId NodeId
  | CmpF NodeId NodeId
  | CmpD NodeId NodeId
  | -- | Bool node
    Bool Comp NodeId
  | -- | Starting point of the control flow subgraph, with own Id
    ParmCtrl NodeId
  | -- | Phi with id and predecessor ids
    Phi
      NodeId
      [NodeId]
  | -- | Region node with list of predecessors
    Region NodeId [NodeId]
  | -- | If node with connection to boolean dataflow node
    If NodeId NodeId
  | -- | IfTrue projection
    IfTrue NodeId
  | -- | IfTrue projection with predecessor
    IfFalse NodeId
  | -- | Return <own nodeid> <dataflow predecessor node id>
    Return NodeId
  | -- | Static calls with own Id data flow predecessor (?)
    CallStatic NodeId NodeId
  deriving (Show, Eq)

data Graph
  = Graph
  { -- | Maps nodeId (SoN id) to Node
    nodeInfo :: M.Map NodeId Node,
    -- | Contains successors for the control flow nodes
    controlSuccessors :: M.Map NodeId [NodeId],
    -- | Keeps track of which predecessor was last used to enter Region node.
    -- Used to determine value given by Phi nodes.
    regionPredecessor :: M.Map NodeId Word32,
    -- | Contains unified variables for parameters.
    -- Both to be reused within the graph and unify between the graphs.
    params :: ParamMap
  }
  deriving (Show)

-- | Symbolic values, showing equivalence between Java values and SMT values
data SValue
  = JInt {getInt :: SInt32}
  | JLong {getLong :: SInt64}
  | -- | FP 8 24
    JFloat {getFloat :: SFPSingle}
  | -- | FP 11 53
    JDouble {getDouble :: SFPDouble}
  deriving (Show, Eq)

instance EqSymbolic SValue where
  (.==) a b =
    case (a, b) of
      (JInt v1, JInt v2) -> v1 .== v2
      (JLong v1, JLong v2) -> v1 .== v2
      (JFloat v1, JFloat v2) -> v1 .== v2
      (JDouble v1, JDouble v2) -> v1 .== v2
      (_, _) -> sFalse

instance Mergeable SValue where
  symbolicMerge force test left right = case (left, right) of
    (JInt x, JInt y) -> JInt $ symbolicMerge force test x y
    (JLong x, JLong y) -> JLong $ symbolicMerge force test x y
    (JFloat x, JFloat y) -> JFloat $ symbolicMerge force test x y
    (JDouble x, JDouble y) -> JDouble $ symbolicMerge force test x y
    (x, y) -> error $ "Cannot merge different SValue types: " <> show x <> " and " <> show y

instance OrdSymbolic SValue where
  (.<) a b =
    case (a, b) of
      (JInt x, JInt y) -> x .< y
      (JLong x, JLong y) -> x .< y
      (JFloat x, JFloat y) -> x .< y
      (JDouble x, JDouble y) -> x .< y
      _ -> error "Cannot compare different SValue types"

-- | Symbolic comparison function
sComp :: SValue -> SValue -> SInt32
sComp x y =
  ite
    (x .< y)
    (literal (-1))
    ( ite
        (x .> y)
        (literal 1)
        (literal 0)
    )

-- | (side effect, return value)
-- e.g. (0, 64) is normal return with value 64
-- Any exception node will instead have their nodeId in the tuple, e.g. (11, ...)
-- for exception on node 11.
type Ret = (SWord32, SValue)

-- Given successor node and id of the current node,
-- if the successor is a region node, update its predecessor with equivalent input index.
updateRegionPredecessors :: Node -> NodeId -> Graph -> Graph
updateRegionPredecessors (Region rnid regionPreds) nid graph@(regionPredecessor -> preds) =
  case (findIndex (== nid) regionPreds) of
    Just idx -> graph {regionPredecessor = M.insert rnid (fromIntegral idx) preds}
    Nothing ->
      error $
        "Couldn't find nid: "
          <> show nid
          <> " amongst predecessors: "
          <> show regionPreds
          <> " for region: "
          <> show rnid
updateRegionPredecessors _ _ g = g

-- | Symbolically executes the control flow subgraph using operational semantics
-- Other than executing control flow, we also indirectly set phi nodes
-- by keeping track of which node enters a region (see @regionPredecessor@).
-- This check is done for @ParmCtrl@, @IfTrue@, @IfFalse@
evalControlNode :: Graph -> Node -> Symbolic Ret
evalControlNode graph (ParmCtrl nid) =
  let [suc] = (controlSuccessors graph) M.! nid
      succNode = (nodeInfo graph) M.! suc
   in evalControlNode
        (updateRegionPredecessors succNode nid graph)
        succNode
evalControlNode graph@(nodeInfo -> nodes) (Return dataId) =
  do
    retVal <- evalDataNode graph (nodes M.! dataId)
    return $ (0, retVal)
evalControlNode graph@(nodeInfo -> nodes) (CallStatic nid dataId) =
  do
    retVal <- evalDataNode graph (nodes M.! dataId)
    return $ (literal nid, retVal)
evalControlNode graph@(nodeInfo -> nodes) (If nid boolGuardId) =
  do
    let [ifNode, elseNode] = (controlSuccessors graph) M.! nid
    -- NOTE: Boolean guards require predecessor to be either 0 (false) or 1 (true)
    boolGuard <- ((.== 1) . getInt) <$> evalDataNode graph (nodes M.! boolGuardId)
    ifBranch <- evalControlNode graph $ nodes M.! ifNode
    elseBranch <- evalControlNode graph $ nodes M.! elseNode
    return $
      ite
        boolGuard
        ifBranch
        elseBranch
evalControlNode graph (IfTrue nid) =
  let [suc] = (controlSuccessors graph) M.! nid
      succNode = (nodeInfo graph) M.! suc
   in evalControlNode
        (updateRegionPredecessors succNode nid graph)
        succNode
evalControlNode graph (IfFalse nid) =
  let [suc] = (controlSuccessors graph) M.! nid
      succNode = (nodeInfo graph) M.! suc
   in evalControlNode
        (updateRegionPredecessors succNode nid graph)
        succNode
evalControlNode graph (Region nid _) =
  let [suc] = (controlSuccessors graph) M.! nid
      succNode = (nodeInfo graph) M.! suc
   in evalControlNode
        (updateRegionPredecessors succNode nid graph)
        succNode
evalControlNode _ node = error $ "Not a control node: " <> show node

-- | Symbolically executed the data flow subgraph using denotational semantics
evalDataNode :: Graph -> Node -> Symbolic SValue
evalDataNode (params -> parms) (ParmI var) = return $ parms M.! var
evalDataNode (params -> parms) (ParmL var) = return $ parms M.! var
evalDataNode (params -> parms) (ParmF var) = return $ parms M.! var
evalDataNode (params -> parms) (ParmD var) = return $ parms M.! var
evalDataNode _ (ConI n) = pure $ JInt $ literal n
evalDataNode _ (ConL n) = pure $ JLong $ literal n
evalDataNode _ (ConF n) = pure $ JFloat $ literal n
evalDataNode _ (ConD n) = pure $ JDouble $ literal n
evalDataNode graph@(nodeInfo -> nodes) (AddI n1 n2) =
  do
    v1 <- getInt <$> evalDataNode graph (nodes M.! n1)
    v2 <- getInt <$> evalDataNode graph (nodes M.! n2)
    return $ JInt (v1 + v2)
evalDataNode graph@(nodeInfo -> nodes) (AddL n1 n2) =
  do
    v1 <- getLong <$> evalDataNode graph (nodes M.! n1)
    v2 <- getLong <$> evalDataNode graph (nodes M.! n2)
    return $ JLong (v1 + v2)
evalDataNode graph@(nodeInfo -> nodes) (AddF n1 n2) =
  do
    v1 <- getFloat <$> evalDataNode graph (nodes M.! n1)
    v2 <- getFloat <$> evalDataNode graph (nodes M.! n2)
    return $ JFloat (v1 + v2)
evalDataNode graph@(nodeInfo -> nodes) (AddD n1 n2) =
  do
    v1 <- getDouble <$> evalDataNode graph (nodes M.! n1)
    v2 <- getDouble <$> evalDataNode graph (nodes M.! n2)
    return $ JDouble (v1 + v2)
evalDataNode graph@(nodeInfo -> nodes) (SubI n1 n2) =
  do
    v1 <- getInt <$> evalDataNode graph (nodes M.! n1)
    v2 <- getInt <$> evalDataNode graph (nodes M.! n2)
    return $ JInt (v1 - v2)
evalDataNode graph@(nodeInfo -> nodes) (SubL n1 n2) =
  do
    v1 <- getLong <$> evalDataNode graph (nodes M.! n1)
    v2 <- getLong <$> evalDataNode graph (nodes M.! n2)
    return $ JLong (v1 - v2)
evalDataNode graph@(nodeInfo -> nodes) (SubF n1 n2) =
  do
    v1 <- getFloat <$> evalDataNode graph (nodes M.! n1)
    v2 <- getFloat <$> evalDataNode graph (nodes M.! n2)
    return $ JFloat (v1 - v2)
evalDataNode graph@(nodeInfo -> nodes) (SubD n1 n2) =
  do
    v1 <- getDouble <$> evalDataNode graph (nodes M.! n1)
    v2 <- getDouble <$> evalDataNode graph (nodes M.! n2)
    return $ JDouble (v1 - v2)
evalDataNode graph@(nodeInfo -> nodes) (MulI n1 n2) =
  do
    v1 <- getInt <$> evalDataNode graph (nodes M.! n1)
    v2 <- getInt <$> evalDataNode graph (nodes M.! n2)
    return $ JInt (v1 * v2)
evalDataNode graph@(nodeInfo -> nodes) (MulL n1 n2) =
  do
    v1 <- getLong <$> evalDataNode graph (nodes M.! n1)
    v2 <- getLong <$> evalDataNode graph (nodes M.! n2)
    return $ JLong (v1 * v2)
evalDataNode graph@(nodeInfo -> nodes) (MulF n1 n2) =
  do
    v1 <- getFloat <$> evalDataNode graph (nodes M.! n1)
    v2 <- getFloat <$> evalDataNode graph (nodes M.! n2)
    return $ JFloat (v1 * v2)
evalDataNode graph@(nodeInfo -> nodes) (MulD n1 n2) =
  do
    v1 <- getDouble <$> evalDataNode graph (nodes M.! n1)
    v2 <- getDouble <$> evalDataNode graph (nodes M.! n2)
    return $ JDouble (v1 * v2)
evalDataNode graph@(nodeInfo -> nodes) (MulHiL n1 n2) =
  -- NOTE: Special mul operation
  -- Given two operands x and y, MulHiL will do the following:
  -- MulHiL x y = upperBits 64 ((signextend x 128) * (signextend y 128))
  do
    l1 <- getLong <$> evalDataNode graph (nodes M.! n1)
    l2 <- getLong <$> evalDataNode graph (nodes M.! n2)

    let sgnl1 = signExtend64To128 l1
    let sgnl2 = signExtend64To128 l2
    let res = extractUpper64Bits $ sgnl1 * sgnl2

    return $ JLong $ fromSized res
  where
    signExtend64To128 :: SInt64 -> SInt 128
    signExtend64To128 = signExtend . toSized
    extractUpper64Bits :: SInt 128 -> SInt 64
    extractUpper64Bits = bvDrop (Proxy @64)
evalDataNode graph@(nodeInfo -> nodes) (DivI n1 n2) =
  do
    v1 <- getInt <$> evalDataNode graph (nodes M.! n1)
    v2 <- getInt <$> evalDataNode graph (nodes M.! n2)
    return $ JInt (v1 `sDiv` v2)
evalDataNode graph@(nodeInfo -> nodes) (DivL n1 n2) =
  do
    v1 <- getLong <$> evalDataNode graph (nodes M.! n1)
    v2 <- getLong <$> evalDataNode graph (nodes M.! n2)
    return $ JLong (v1 `sDiv` v2)
evalDataNode graph@(nodeInfo -> nodes) (DivF n1 n2) =
  do
    v1 <- getFloat <$> evalDataNode graph (nodes M.! n1)
    v2 <- getFloat <$> evalDataNode graph (nodes M.! n2)
    return $ JFloat (v1 / v2)
evalDataNode graph@(nodeInfo -> nodes) (DivD n1 n2) =
  do
    v1 <- getDouble <$> evalDataNode graph (nodes M.! n1)
    v2 <- getDouble <$> evalDataNode graph (nodes M.! n2)
    return $ JDouble (v1 / v2)
evalDataNode graph@(nodeInfo -> nodes) (AndI n1 n2) =
  do
    v1 <- getInt <$> evalDataNode graph (nodes M.! n1)
    v2 <- getInt <$> evalDataNode graph (nodes M.! n2)
    return $ JInt (v1 .&. v2)
evalDataNode graph@(nodeInfo -> nodes) (AndL n1 n2) =
  do
    v1 <- getLong <$> evalDataNode graph (nodes M.! n1)
    v2 <- getLong <$> evalDataNode graph (nodes M.! n2)
    return $ JLong (v1 .&. v2)
evalDataNode graph@(nodeInfo -> nodes) (OrI n1 n2) =
  do
    v1 <- getInt <$> evalDataNode graph (nodes M.! n1)
    v2 <- getInt <$> evalDataNode graph (nodes M.! n2)
    return $ JInt (v1 .|. v2)
evalDataNode graph@(nodeInfo -> nodes) (OrL n1 n2) =
  do
    v1 <- getLong <$> evalDataNode graph (nodes M.! n1)
    v2 <- getLong <$> evalDataNode graph (nodes M.! n2)
    return $ JLong (v1 .|. v2)
evalDataNode graph@(nodeInfo -> nodes) (LShiftI n1 n2) =
  -- NOTE: LShift has special semantics in the JVM
  -- int: n1 << (0x1f & n2)
  do
    v1 <- getInt <$> evalDataNode graph (nodes M.! n1)
    v2 <- getInt <$> evalDataNode graph (nodes M.! n2)
    return $ JInt (v1 `sShiftLeft` (literal 0x1f .&. v2))
evalDataNode graph@(nodeInfo -> nodes) (LShiftL n1 n2) =
  -- NOTE: LShift has special semantics in the JVM
  -- long: n1 << (0x3f & n2)
  do
    v1 <- getLong <$> evalDataNode graph (nodes M.! n1)
    v2 <- getInt <$> evalDataNode graph (nodes M.! n2)
    return $ JLong (v1 `sShiftLeft` (literal 0x3f .&. v2))
evalDataNode graph@(nodeInfo -> nodes) (RShiftI n1 n2) =
  -- NOTE: RShift has special semantics in the JVM
  -- int: n1 >> (0x1f & n2)
  do
    v1 <- getInt <$> evalDataNode graph (nodes M.! n1)
    v2 <- getInt <$> evalDataNode graph (nodes M.! n2)
    return $ JInt (v1 `sShiftRight` (literal 0x1f .&. v2))
evalDataNode graph@(nodeInfo -> nodes) (RShiftL n1 n2) =
  -- NOTE: RShift has special semantics in the JVM
  -- long: n1 >> (0x3f & n2)
  do
    v1 <- getLong <$> evalDataNode graph (nodes M.! n1)
    v2 <- getInt <$> evalDataNode graph (nodes M.! n2)
    return $ JLong (v1 `sShiftRight` (literal 0x3f .&. v2))
-- NOTE: The compare semantics are:
-- Cmp n1 n2
-- n1 == n2 -> 0
-- n1 > n2 -> 1
-- n1 < n2 -> -1
-- The result is always an integer
evalDataNode graph@(nodeInfo -> nodes) (CmpI n1 n2) =
  do
    v1 <- evalDataNode graph (nodes M.! n1)
    v2 <- evalDataNode graph (nodes M.! n2)
    return $ JInt $ sComp v1 v2
evalDataNode graph@(nodeInfo -> nodes) (CmpL n1 n2) =
  do
    v1 <- evalDataNode graph (nodes M.! n1)
    v2 <- evalDataNode graph (nodes M.! n2)
    return $ JInt $ sComp v1 v2
evalDataNode graph@(nodeInfo -> nodes) (CmpF n1 n2) =
  do
    v1 <- evalDataNode graph (nodes M.! n1)
    v2 <- evalDataNode graph (nodes M.! n2)
    return $ JInt $ sComp v1 v2
evalDataNode graph@(nodeInfo -> nodes) (CmpD n1 n2) =
  do
    v1 <- evalDataNode graph (nodes M.! n1)
    v2 <- evalDataNode graph (nodes M.! n2)
    return $ JInt $ sComp v1 v2
evalDataNode graph@(nodeInfo -> nodes) (Bool cmp n) =
  -- NOTE: Bool cmp n
  -- cmp = ne, return n == 0
  do
    v <- getInt <$> evalDataNode graph (nodes M.! n)
    case cmp of
      Ne -> return $ JInt $ ite (v .== 0) (literal 1) (literal 0)
evalDataNode graph (Phi rid preds) =
  -- The phi node's value depends on the corresponding region node
  let dataIdx = (regionPredecessor graph) M.! rid
      chosenDataNode = (nodeInfo graph) M.! (preds !! fromIntegral dataIdx)
   in return =<< evalDataNode graph chosenDataNode
evalDataNode _ n = error $ "Not a data node:" <> show n

mkGraph :: NodeInfo -> M.Map NodeId [NodeId] -> Graph
mkGraph nInfo successors =
  Graph
    { nodeInfo = nInfo,
      controlSuccessors = successors,
      regionPredecessor = M.empty,
      params = M.empty
    }

-- | Creates a symbolic value for each parameter node.
-- The symbolic values will be reused in the SMT formulas
createParams :: Graph -> Symbolic ParamMap
createParams (nodeInfo -> nodes) = go M.empty (map snd $ M.toList nodes)
  where
    go paramMap [] = return paramMap
    go paramMap (x : rest) =
      case x of
        ParmI nid ->
          do
            param <- JInt <$> sInt32 ("parm" <> show nid)
            go (M.insert nid param paramMap) rest
        ParmL nid ->
          do
            param <- JLong <$> sInt64 ("parm" <> show nid)
            go (M.insert nid param paramMap) rest
        ParmF nid ->
          do
            param <- JFloat <$> sFPSingle ("parm" <> show nid)
            go (M.insert nid param paramMap) rest
        ParmD nid ->
          do
            param <- JDouble <$> sFPDouble ("parm" <> show nid)
            go (M.insert nid param paramMap) rest
        _ -> go paramMap rest

main :: IO ()
main =
  do
    let before = phiBefore
    let after = phiAfter
    res <- satWith z3 {verbose = True, timing = PrintTiming} $
      do
        parms <- createParams before
        res1 <- evalControlNode (before {params = parms}) (ParmCtrl 5)
        res2 <- evalControlNode (after {params = parms}) (ParmCtrl 5)
        -- NOTE: Strong equality, e.g. NaN == NaN but -0 /= +0
        constrain $ res1 ./== res2
    print res

phiBefore :: Graph
phiBefore =
  mkGraph
    ( M.fromList
        [ (5, ParmCtrl 5),
          (10, ParmI 10),
          (11, ParmI 11),
          (23, ConI 0),
          (24, SubI 23 10),
          (25, SubI 23 11),
          (26, AndI 24 25),
          (27, ConI (-2)),
          (28, CmpI 26 27),
          (29, Bool Ne 28),
          (5, ParmCtrl 5),
          (30, If 30 29),
          (31, IfTrue 31),
          (32, IfFalse 32),
          (14, Region 14 [32, 31]),
          (36, ConI 140),
          (37, ConI 120),
          (18, Phi 14 [36, 37]),
          (38, Return 18)
        ]
    )
    ( M.fromList
        [ (5, [30]),
          (30, [31, 32]),
          (32, [14]),
          (31, [14]),
          (14, [38])
        ]
    )

phiAfter :: Graph
phiAfter =
  mkGraph
    ( M.fromList
        [ (5, ParmCtrl 5),
          (10, ParmI 10),
          (11, ParmI 11),
          (26, AndI 10 11),
          (27, ConI (-2)),
          (28, CmpI 26 27),
          (29, Bool Ne 28),
          (5, ParmCtrl 5),
          (30, If 30 29),
          (31, IfTrue 31),
          (32, IfFalse 32),
          (14, Region 14 [32, 31]),
          (36, ConI 140),
          (37, ConI 120),
          (18, Phi 14 [36, 37]),
          (38, Return 18)
        ]
    )
    ( M.fromList
        [ (5, [30]),
          (30, [31, 32]),
          (32, [14]),
          (31, [14]),
          (14, [38])
        ]
    )

sideBefore :: Graph
sideBefore =
  mkGraph
    ( M.fromList
        [ (10, ParmI 10),
          (11, ParmI 11),
          (23, ConI 0),
          (24, SubI 23 10),
          (25, SubI 23 11),
          (26, AndI 24 25),
          (27, ConI (-2)),
          (28, CmpI 26 27),
          (29, Bool Ne 28),
          (5, ParmCtrl 5),
          (30, If 30 29),
          (31, IfTrue 31),
          (32, IfFalse 32),
          (37, CallStatic 37 42),
          (43, Return 42),
          (42, ConI 120)
        ]
    )
    ( M.fromList
        [ (5, [30]),
          (30, [31, 32]),
          (32, [37]),
          (31, [43])
        ]
    )

sideAfter :: Graph
sideAfter =
  mkGraph
    ( M.fromList
        [ (10, ParmI 10),
          (11, ParmI 11),
          (26, AndI 10 11),
          (27, ConI (-2)),
          (28, CmpI 26 27),
          (29, Bool Ne 28),
          (5, ParmCtrl 5),
          (30, If 30 29),
          (31, IfTrue 31),
          (32, IfFalse 32),
          (37, CallStatic 37 42),
          (43, Return 42),
          (42, ConI 120)
        ]
    )
    ( M.fromList
        [ (5, [30]),
          (30, [31, 32]),
          (32, [37]),
          (31, [43])
        ]
    )

testGraph :: Graph
testGraph =
  mkGraph
    (M.fromList [(1, ConI 14), (2, ConI 15), (3, AddI 1 2), (4, ParmI 4)])
    M.empty

binopGraph :: Graph
binopGraph =
  mkGraph
    ( M.fromList
        [ (29, Return 28),
          (28, AddI 25 27),
          (25, MulI 10 11),
          (27, AddI 10 10),
          (10, ParmI 10),
          (11, ParmI 11)
        ]
    )
    M.empty

andNegBefore :: Graph
andNegBefore =
  mkGraph
    ( M.fromList
        [ (10, ParmI 10),
          (11, ParmI 11),
          (23, ConI 0),
          (24, SubI 23 10),
          (25, SubI 23 11),
          (26, AndI 24 25),
          (27, Return 26)
        ]
    )
    M.empty

andNegAfter :: Graph
andNegAfter =
  mkGraph
    ( M.fromList
        [ (10, ParmI 10),
          (11, ParmI 11),
          (26, AndI 10 11),
          (27, Return 26)
        ]
    )
    M.empty

lshiftBefore :: Graph
lshiftBefore =
  mkGraph
    ( M.fromList
        [ (10, ParmL 10),
          (23, AddL 10 10),
          (24, ConI 63),
          (25, LShiftL 23 24),
          (26, Return 25)
        ]
    )
    M.empty

lshiftAfter :: Graph
lshiftAfter =
  mkGraph
    ( M.fromList
        [ (10, ParmL 10),
          (26, Return 10)
        ]
    )
    M.empty

mulassoBefore :: Graph
mulassoBefore =
  mkGraph
    ( M.fromList
        [ (10, ParmF 10),
          (22, ConF 00111101111110111110011101101101),
          (23, ConF 00111111100111010111000010100100),
          (24, MulF 10 22),
          (25, MulF 24 23),
          (26, Return 25)
        ]
    )
    M.empty

mulassoAfter :: Graph
mulassoAfter =
  mkGraph
    ( M.fromList
        [ (10, ParmF 10),
          (27, ConF 00111110000110101110101111000100),
          (25, MulF 10 27),
          (26, Return 25)
        ]
    )
    M.empty

mulFloatBefore :: Graph
mulFloatBefore =
  mkGraph
    ( M.fromList
        [ (5, ParmCtrl 5),
          (10, ParmF 10),
          (22, ConF 0),
          (23, MulF 22 10),
          (24, ConF 01000000000000000000000000000000),
          (28, CmpF 10 24),
          (29, Bool Ne 28),
          (35, ConI 25),
          (36, CallStatic 36 23),
          (30, If 30 29),
          (31, IfTrue 31),
          (32, IfFalse 32),
          (41, Return 23)
        ]
    )
    ( M.fromList
        [ (5, [30]),
          (30, [31, 32]),
          (32, [36]),
          (31, [41])
        ]
    )

mulFloatAfter :: Graph
mulFloatAfter =
  mkGraph
    ( M.fromList
        [ (5, ParmCtrl 5),
          (10, ParmF 10),
          (22, ConF 0),
          (24, ConF 01000000000000000000000000000000),
          (28, CmpF 10 24),
          (29, Bool Ne 28),
          (35, ConI 25),
          (36, CallStatic 36 22),
          (30, If 30 29),
          (31, IfTrue 31),
          (32, IfFalse 32),
          (41, Return 22)
        ]
    )
    ( M.fromList
        [ (5, [30]),
          (30, [31, 32]),
          (32, [36]),
          (31, [41])
        ]
    )

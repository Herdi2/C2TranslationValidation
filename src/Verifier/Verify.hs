{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Verifier.Verify where

import Data.List (findIndex)
import qualified Data.Map as M
import Data.Proxy
import Data.SBV
import Verifier.Graph

-- | Partial accessor functions, to help indicate any type errors during symbolic execution
getInt :: SValue -> SInt32
getInt (JInt v) = v
getInt sval = error $ "getInt: Got " <> show sval

getLong :: SValue -> SInt64
getLong (JLong v) = v
getLong sval = error $ "getLong: Got " <> show sval

getFloat :: SValue -> SFloat
getFloat (JFloat v) = v
getFloat sval = error $ "getFloat: Got " <> show sval

getDouble :: SValue -> SDouble
getDouble (JDouble v) = v
getDouble sval = error $ "getDouble: Got " <> show sval

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
evalControlNode graph@(nodeInfo -> nodes) (Return nid dataId) =
  do
    retVal <- evalDataNode graph (nodes M.! dataId)
    return $ (0, retVal)
evalControlNode graph@(nodeInfo -> nodes) (CallStatic nid dataId) =
  do
    retVal <- evalDataNode graph (nodes M.! dataId)
    return $ (literal nid, retVal)
evalControlNode graph@(nodeInfo -> nodes) (If nid boolGuardId) =
  do
    let [elseNode, ifNode] = (controlSuccessors graph) M.! nid
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
    extractUpper64Bits = bvExtract (Proxy @127) (Proxy @64)
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
-- The floating-point conversions are taken from https://docs.oracle.com/javase/specs/jvms/se25/html/jvms-2.html#jvms-2.8
-- Conversion to an integer value uses IEEE 754 roundTowardZero
-- Any other conversion uses IEEE 754 roundTiesToEven
-- Conversions to integer values, IEEE 754 roundTowardsZero:
evalDataNode graph@(nodeInfo -> nodes) (ConvD2I nid) =
  do
    v <- getDouble <$> evalDataNode graph (nodes M.! nid)
    return $ JInt $ fromSDouble sRoundTowardZero v
evalDataNode graph@(nodeInfo -> nodes) (ConvD2L nid) =
  do
    v <- getDouble <$> evalDataNode graph (nodes M.! nid)
    return $ JLong $ fromSDouble sRoundTowardZero v
evalDataNode graph@(nodeInfo -> nodes) (ConvF2I nid) =
  do
    v <- getFloat <$> evalDataNode graph (nodes M.! nid)
    return $ JInt $ fromSFloat sRoundTowardZero v
evalDataNode graph@(nodeInfo -> nodes) (ConvF2L nid) =
  do
    v <- getFloat <$> evalDataNode graph (nodes M.! nid)
    return $ JLong $ fromSFloat sRoundTowardZero v
-- Other conversions, IEEE 754 roundTiesToEven:
evalDataNode graph@(nodeInfo -> nodes) (ConvF2D nid) =
  do
    v <- getFloat <$> evalDataNode graph (nodes M.! nid)
    return $ JDouble $ fromSFloat sRoundNearestTiesToEven v
evalDataNode graph@(nodeInfo -> nodes) (ConvD2F nid) =
  do
    v <- getDouble <$> evalDataNode graph (nodes M.! nid)
    return $ JFloat $ fromSDouble sRoundNearestTiesToEven v
evalDataNode graph@(nodeInfo -> nodes) (ConvI2D nid) =
  do
    v <- getInt <$> evalDataNode graph (nodes M.! nid)
    return $ JDouble $ toSDouble sRoundNearestTiesToEven v
evalDataNode graph@(nodeInfo -> nodes) (ConvI2F nid) =
  do
    v <- getInt <$> evalDataNode graph (nodes M.! nid)
    return $ JFloat $ toSFloat sRoundNearestTiesToEven v
evalDataNode graph@(nodeInfo -> nodes) (ConvL2D nid) =
  do
    v <- getLong <$> evalDataNode graph (nodes M.! nid)
    return $ JDouble $ toSDouble sRoundNearestTiesToEven v
evalDataNode graph@(nodeInfo -> nodes) (ConvL2F nid) =
  do
    v <- getLong <$> evalDataNode graph (nodes M.! nid)
    return $ JFloat $ toSFloat sRoundNearestTiesToEven v
-- Conversion between integer and long https://docs.oracle.com/javase/specs/jvms/se25/html/jvms-2.html#jvms-2.11.4
-- Widening operation, simple sign extension
evalDataNode graph@(nodeInfo -> nodes) (ConvI2L nid) =
  do
    v <- getInt <$> evalDataNode graph (nodes M.! nid)
    return $ JLong $ signExtend32To64 v
  where
    signExtend32To64 :: SInt32 -> SInt64
    signExtend32To64 = fromSized . (signExtend :: SInt 32 -> SInt 64) . toSized
-- Narrowing operation, discard all but the lower 32 bits
evalDataNode graph@(nodeInfo -> nodes) (ConvL2I nid) =
  do
    v <- getLong <$> evalDataNode graph (nodes M.! nid)
    return $ JInt $ dropUpper32 v
  where
    dropUpper32 :: SInt64 -> SInt32
    dropUpper32 = fromSized . (bvDrop (Proxy @32) :: SInt 64 -> SInt 32) . toSized
evalDataNode _ n = error $ "Not a data node:" <> show n

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
            param <- JFloat <$> sFloat ("parm" <> show nid)
            go (M.insert nid param paramMap) rest
        ParmD nid ->
          do
            param <- JDouble <$> sDouble ("parm" <> show nid)
            go (M.insert nid param paramMap) rest
        _ -> go paramMap rest

runVerification :: Graph -> Graph -> IO SatResult
runVerification before after =
  satWith z3 {verbose = True, timing = PrintTiming} $
    do
      parms <- createParams before
      res1 <- evalControlNode (before {params = parms}) (ParmCtrl 5)
      res2 <- evalControlNode (after {params = parms}) (ParmCtrl 5)
      -- NOTE: Strong equality, e.g. NaN == NaN but -0 /= +0
      constrain $ res1 ./== res2

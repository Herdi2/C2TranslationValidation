{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Verifier.Verify where

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.List (findIndex)
import qualified Data.Map as M
import Data.Proxy
import Data.SBV
import Debug.Trace
import Verifier.ErrorHandler
import Verifier.Graph

-- | Proper error message for search key in map
(!!!) :: forall k a. (Show k, Show a, Ord k) => M.Map k a -> k -> a
(!!!) m k =
  case (M.!?) m k of
    Just res -> res
    Nothing -> error $ "(!!!): Could not find key " <> show k <> " in map " <> show m

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

-- | Symbolic unsigned comparison function
uComp :: SValue -> SValue -> SInt32
uComp (getInt -> x) (getInt -> y) =
  let ux = sFromIntegral x :: SWord32
      uy = sFromIntegral y :: SWord32
   in ite
        (ux .< uy)
        (literal (-1))
        ( ite
            (ux .> uy)
            (literal 1)
            (literal 0)
        )

-- | (SIDE_EFFECT, RET_VAL)
-- SIDE_EFFECT will be the node id in which the side effect happens.
-- Side effect nodes are either the return node or any `CallStatic` node.
-- The RET_VAL will only have a return value when we reach the `Return` node.
-- Otherwise, the value is `0` as we do not care for values returned by `CallStatic` nodes.
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
  case controlSuccessors graph !!! nid of
    [suc] ->
      let succNode = (nodeInfo graph) !!! suc
       in evalControlNode
            (updateRegionPredecessors succNode nid graph)
            succNode
    succs -> raiseException $ "ParmCtrl: Expected one succesor but got " <> show (length succs)
evalControlNode graph@(nodeInfo -> nodes) (Return nid dataId) =
  do
    retVal <- evalDataNode graph (nodes !!! dataId)
    return $ (literal nid, retVal)
evalControlNode graph (CallStatic nid) =
  -- NOTE: The return value of a static call is always set to 0, as
  -- we are interested in the node we reach in this case, not the value!
  let retType = methodType graph
   in return $ (literal nid, mkRetValue retType)
evalControlNode graph (Rethrow nid) =
  -- NOTE: Similar motivation as @CallStatic@
  let retType = methodType graph
   in return $ (literal nid, mkRetValue retType)
evalControlNode graph@(nodeInfo -> nodes) (If nid boolGuardId) =
  do
    (elseNode, ifNode) <-
      -- NOTE: Node Id of the false branch is always greater than the true branch
      case (controlSuccessors graph) !!! nid of
        [a, b] ->
          let (a', b') = (nodes !!! a, nodes !!! b)
              isTrueBranch (IfTrue _) = True
              isTrueBranch _ = False
           in if isTrueBranch a'
                then return (b', a')
                else return (a', b')
        other -> raiseException $ "Ifnode " <> show nid <> ": Expected 2 successors but got " <> show other
    -- NOTE: Boolean guards require predecessor to be either 0 (false) or 1 (true)
    boolGuard <- ((.== JInt 1)) <$> evalDataNode graph (nodes !!! boolGuardId)
    ifBranch <- evalControlNode graph $ ifNode
    elseBranch <- evalControlNode graph $ elseNode
    return $
      ite
        boolGuard
        ifBranch
        elseBranch
evalControlNode graph (IfTrue nid) =
  case (controlSuccessors graph) !!! nid of
    [suc] ->
      let succNode = (nodeInfo graph) !!! suc
       in evalControlNode
            (updateRegionPredecessors succNode nid graph)
            succNode
    succs ->
      raiseException $
        "IfTrue: expected one successor but got: "
          <> show (length succs)
evalControlNode graph (IfFalse nid) =
  case (controlSuccessors graph) !!! nid of
    [suc] ->
      let succNode = (nodeInfo graph) !!! suc
       in evalControlNode
            (updateRegionPredecessors succNode nid graph)
            succNode
    succs ->
      raiseException $
        "IfFalse: expected one successor but got: "
          <> show (length succs)
evalControlNode graph (Region nid _) =
  case (controlSuccessors graph) !!! nid of
    [suc] ->
      let succNode = (nodeInfo graph) !!! suc
       in evalControlNode
            (updateRegionPredecessors succNode nid graph)
            succNode
    succs -> raiseException $ "Region node: expected one successor, got: " <> show (length succs)
evalControlNode _ node = raiseException $ "Not a control node: " <> show node

-- | Symbolically executed the data flow subgraph using denotational semantics
-- NOTE: Very important regarding floating point and doubles.
-- Using e.g. `fpAdd` with explicit rounding modes to capture the correct semantics
-- of floating point operations generates SMT formulas that take a very long time.
-- I do not see why I shouldn't simply use (+) instead.
-- No false positives yet.
evalDataNode :: Graph -> Node -> Symbolic SValue
evalDataNode (params -> parms) (ParmI var) = return $ parms !!! var
evalDataNode (params -> parms) (ParmL var) = return $ parms !!! var
evalDataNode (params -> parms) (ParmF var) = return $ parms !!! var
evalDataNode (params -> parms) (ParmD var) = return $ parms !!! var
evalDataNode _ (ParmMemPtr _ base) = return $ base
evalDataNode _ (CastII val) = return val
evalDataNode _ (ConI n) = pure $ JInt $ literal n
evalDataNode _ (ConL n) = pure $ JLong $ literal n
evalDataNode _ (ConF n) = pure $ JFloat $ literal n
evalDataNode _ (ConD n) = pure $ JDouble $ literal n
evalDataNode _ (ConP _) = raiseException "evalDataNode: Independently evaluated ConP node"
evalDataNode graph@(nodeInfo -> nodes) (AddI n1 n2) =
  do
    v1 <- getInt <$> evalDataNode graph (nodes !!! n1)
    v2 <- getInt <$> evalDataNode graph (nodes !!! n2)
    return $ JInt (v1 + v2)
evalDataNode graph@(nodeInfo -> nodes) (AddL n1 n2) =
  do
    v1 <- getLong <$> evalDataNode graph (nodes !!! n1)
    v2 <- getLong <$> evalDataNode graph (nodes !!! n2)
    return $ JLong (v1 + v2)
evalDataNode graph@(nodeInfo -> nodes) (AddF n1 n2) =
  do
    v1 <- getFloat <$> evalDataNode graph (nodes !!! n1)
    v2 <- getFloat <$> evalDataNode graph (nodes !!! n2)
    return $ JFloat (v1 + v2)
evalDataNode graph@(nodeInfo -> nodes) (AddD n1 n2) =
  do
    v1 <- getDouble <$> evalDataNode graph (nodes !!! n1)
    v2 <- getDouble <$> evalDataNode graph (nodes !!! n2)
    return $ JDouble (v1 + v2)
evalDataNode graph@(nodeInfo -> nodes) (SubI n1 n2) =
  do
    v1 <- getInt <$> evalDataNode graph (nodes !!! n1)
    v2 <- getInt <$> evalDataNode graph (nodes !!! n2)
    return $ JInt (v1 - v2)
evalDataNode graph@(nodeInfo -> nodes) (SubL n1 n2) =
  do
    v1 <- getLong <$> evalDataNode graph (nodes !!! n1)
    v2 <- getLong <$> evalDataNode graph (nodes !!! n2)
    return $ JLong (v1 - v2)
evalDataNode graph@(nodeInfo -> nodes) (SubF n1 n2) =
  do
    v1 <- getFloat <$> evalDataNode graph (nodes !!! n1)
    v2 <- getFloat <$> evalDataNode graph (nodes !!! n2)
    return $ JFloat (v1 - v2)
evalDataNode graph@(nodeInfo -> nodes) (SubD n1 n2) =
  do
    v1 <- getDouble <$> evalDataNode graph (nodes !!! n1)
    v2 <- getDouble <$> evalDataNode graph (nodes !!! n2)
    return $ JDouble (v1 - v2)
evalDataNode graph@(nodeInfo -> nodes) (MulI n1 n2) =
  do
    v1 <- getInt <$> evalDataNode graph (nodes !!! n1)
    v2 <- getInt <$> evalDataNode graph (nodes !!! n2)
    return $ JInt (v1 * v2)
evalDataNode graph@(nodeInfo -> nodes) (MulL n1 n2) =
  do
    v1 <- getLong <$> evalDataNode graph (nodes !!! n1)
    v2 <- getLong <$> evalDataNode graph (nodes !!! n2)
    return $ JLong (v1 * v2)
evalDataNode graph@(nodeInfo -> nodes) (MulF n1 n2) =
  do
    v1 <- getFloat <$> evalDataNode graph (nodes !!! n1)
    v2 <- getFloat <$> evalDataNode graph (nodes !!! n2)
    return $ JFloat (v1 * v2)
evalDataNode graph@(nodeInfo -> nodes) (MulD n1 n2) =
  do
    v1 <- getDouble <$> evalDataNode graph (nodes !!! n1)
    v2 <- getDouble <$> evalDataNode graph (nodes !!! n2)
    return $ JDouble (v1 * v2)
evalDataNode graph@(nodeInfo -> nodes) (MulHiL n1 n2) =
  -- NOTE: Special mul operation
  -- Given two operands x and y, MulHiL will do the following:
  -- MulHiL x y = upperBits 64 ((signextend x 128) * (signextend y 128))
  do
    l1 <- getLong <$> evalDataNode graph (nodes !!! n1)
    l2 <- getLong <$> evalDataNode graph (nodes !!! n2)

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
    v1 <- getInt <$> evalDataNode graph (nodes !!! n1)
    v2 <- getInt <$> evalDataNode graph (nodes !!! n2)
    -- NOTE: We use `sQuot` since division rounds towards zero
    -- https://docs.oracle.com/javase/specs/jvms/se25/html/jvms-6.html#jvms-6.5.idiv
    return $ JInt (v1 `sQuot` v2)
evalDataNode graph@(nodeInfo -> nodes) (DivL n1 n2) =
  do
    v1 <- getLong <$> evalDataNode graph (nodes !!! n1)
    v2 <- getLong <$> evalDataNode graph (nodes !!! n2)
    -- NOTE: We use `sQuot` since division rounds towards zero
    -- https://docs.oracle.com/javase/specs/jvms/se25/html/jvms-6.html#jvms-6.5.ldiv
    return $ JLong (v1 `sQuot` v2)
evalDataNode graph@(nodeInfo -> nodes) (DivF n1 n2) =
  do
    v1 <- getFloat <$> evalDataNode graph (nodes !!! n1)
    v2 <- getFloat <$> evalDataNode graph (nodes !!! n2)
    return $ JFloat (v1 / v2)
evalDataNode graph@(nodeInfo -> nodes) (DivD n1 n2) =
  do
    v1 <- getDouble <$> evalDataNode graph (nodes !!! n1)
    v2 <- getDouble <$> evalDataNode graph (nodes !!! n2)
    -- https://docs.oracle.com/javase/specs/jvms/se25/html/jvms-6.html#jvms-6.5.ddiv
    -- IEEE 754 arithmetic, round to nearest policy
    return $ JDouble (v1 / v2)
evalDataNode graph@(nodeInfo -> nodes) (AndI n1 n2) =
  do
    v1 <- getInt <$> evalDataNode graph (nodes !!! n1)
    v2 <- getInt <$> evalDataNode graph (nodes !!! n2)
    return $ JInt (v1 .&. v2)
evalDataNode graph@(nodeInfo -> nodes) (AndL n1 n2) =
  do
    v1 <- getLong <$> evalDataNode graph (nodes !!! n1)
    v2 <- getLong <$> evalDataNode graph (nodes !!! n2)
    return $ JLong (v1 .&. v2)
evalDataNode graph@(nodeInfo -> nodes) (OrI n1 n2) =
  do
    v1 <- getInt <$> evalDataNode graph (nodes !!! n1)
    v2 <- getInt <$> evalDataNode graph (nodes !!! n2)
    return $ JInt (v1 .|. v2)
evalDataNode graph@(nodeInfo -> nodes) (OrL n1 n2) =
  do
    v1 <- getLong <$> evalDataNode graph (nodes !!! n1)
    v2 <- getLong <$> evalDataNode graph (nodes !!! n2)
    return $ JLong (v1 .|. v2)
evalDataNode graph@(nodeInfo -> nodes) (XorI n1 n2) =
  do
    v1 <- getInt <$> evalDataNode graph (nodes !!! n1)
    v2 <- getInt <$> evalDataNode graph (nodes !!! n2)
    return $ JInt (v1 `xor` v2)
evalDataNode graph@(nodeInfo -> nodes) (XorL n1 n2) =
  do
    v1 <- getLong <$> evalDataNode graph (nodes !!! n1)
    v2 <- getLong <$> evalDataNode graph (nodes !!! n2)
    return $ JLong (v1 `xor` v2)
evalDataNode graph@(nodeInfo -> nodes) (LShiftI n1 n2) =
  -- NOTE: LShift has special semantics in the JVM
  -- int: n1 << (0x1f & n2)
  -- 12669110630253530122
  do
    v1 <- getInt <$> evalDataNode graph (nodes !!! n1)
    v2 <- getInt <$> evalDataNode graph (nodes !!! n2)
    return $ JInt (v1 `sShiftLeft` (literal 0x1f .&. v2))
evalDataNode graph@(nodeInfo -> nodes) (LShiftL n1 n2) =
  -- NOTE: LShift has special semantics in the JVM
  -- long: n1 << (0x3f & n2)
  do
    v1 <- getLong <$> evalDataNode graph (nodes !!! n1)
    v2 <- getInt <$> evalDataNode graph (nodes !!! n2)
    return $ JLong (v1 `sShiftLeft` (literal 0x3f .&. v2))
evalDataNode graph@(nodeInfo -> nodes) (RShiftI n1 n2) =
  -- NOTE: RShift has special semantics in the JVM
  -- int: n1 >> (0x1f & n2)
  do
    v1 <- getInt <$> evalDataNode graph (nodes !!! n1)
    v2 <- getInt <$> evalDataNode graph (nodes !!! n2)
    return $ JInt (v1 `sShiftRight` (literal 0x1f .&. v2))
evalDataNode graph@(nodeInfo -> nodes) (RShiftL n1 n2) =
  -- NOTE: RShift has special semantics in the JVM
  -- long: n1 >> (0x3f & n2)
  do
    v1 <- getLong <$> evalDataNode graph (nodes !!! n1)
    v2 <- getInt <$> evalDataNode graph (nodes !!! n2)
    return $ JLong (v1 `sShiftRight` (literal 0x3f .&. v2))
-- NOTE: The compare semantics are:
-- Cmp n1 n2
-- n1 == n2 -> 0
-- n1 > n2 -> 1
-- n1 < n2 -> -1
-- The result is always an integer
evalDataNode graph@(nodeInfo -> nodes) (CmpI n1 n2) =
  do
    v1 <- evalDataNode graph (nodes !!! n1)
    v2 <- evalDataNode graph (nodes !!! n2)
    return $ JInt $ sComp v1 v2
evalDataNode graph@(nodeInfo -> nodes) (CmpL n1 n2) =
  do
    v1 <- evalDataNode graph (nodes !!! n1)
    v2 <- evalDataNode graph (nodes !!! n2)
    return $ JInt $ sComp v1 v2
evalDataNode graph@(nodeInfo -> nodes) (CmpF n1 n2) =
  do
    v1 <- evalDataNode graph (nodes !!! n1)
    v2 <- evalDataNode graph (nodes !!! n2)
    return $ JInt $ sComp v1 v2
evalDataNode graph@(nodeInfo -> nodes) (CmpD n1 n2) =
  do
    v1 <- evalDataNode graph (nodes !!! n1)
    v2 <- evalDataNode graph (nodes !!! n2)
    return $ JInt $ sComp v1 v2
evalDataNode graph@(nodeInfo -> nodes) (CmpU n1 n2) =
  do
    v1 <- evalDataNode graph (nodes !!! n1)
    v2 <- evalDataNode graph (nodes !!! n2)
    return $ JInt $ uComp v1 v2
evalDataNode graph@(nodeInfo -> nodes) (CmpUL n1 n2) =
  do
    v1 <- evalDataNode graph (nodes !!! n1)
    v2 <- evalDataNode graph (nodes !!! n2)
    return $ JInt $ uComp v1 v2
evalDataNode graph@(nodeInfo -> nodes) (CmpP n1 n2) =
  do
    -- Since we do not use objects other than when comparing to null
    -- pointers, we will always assume exactly one input is null
    -- Therefore, we let the object's "nullness" be determined by
    -- the SMT solver, to allow for faulty control flow related to null-checking.
    nonNullNode <-
      case (nodes !!! n1, nodes !!! n2) of
        (ConP _, ConP _) -> raiseException "Malformed graph, two ConP into CmpP"
        (ConP "null", nonNull) -> return nonNull
        (nonNull, ConP "null") -> return nonNull
        (_, _) -> raiseException "Malformed graph, two non-null into CmpP"
    address <- evalDataNode graph nonNullNode
    let nullness = (_pointerMem graph) (literal $ showptr address)
    -- Recall, if its null then CmpP returns equality, which is 0. Otherwise, 1.
    return $ JInt $ ite (nullness) (literal 0) (literal 1)
evalDataNode graph@(nodeInfo -> nodes) (Bool cmp n) =
  do
    v <- getInt <$> evalDataNode graph (nodes !!! n)
    case cmp of
      Ne -> return $ JInt $ ite (v .== 0) (literal 0) (literal 1)
      Ee -> return $ JInt $ ite (v .== 0) (literal 1) (literal 0)
      Le -> return $ JInt $ ite (v .== -1 .|| v .== 0) (literal 1) (literal 0)
      Lt -> return $ JInt $ ite (v .== -1) (literal 1) (literal 0)
      Gt -> return $ JInt $ ite (v .== 1) (literal 1) (literal 0)
      Ge -> return $ JInt $ ite (v .== 1 .|| v .== 0) (literal 1) (literal 0)
evalDataNode graph (CMoveI n1 n2) = handleCMove graph n1 n2
evalDataNode graph (CMoveL n1 n2) = handleCMove graph n1 n2
evalDataNode graph (CMoveF n1 n2) = handleCMove graph n1 n2
evalDataNode graph (CMoveD n1 n2) = handleCMove graph n1 n2
evalDataNode graph (Phi rid preds) =
  -- The phi node's value depends on the corresponding region node
  let dataIdx = (regionPredecessor graph) !!! rid
      chosenDataNode = (nodeInfo graph) !!! (preds !! fromIntegral dataIdx)
   in evalDataNode graph chosenDataNode
-- The floating-point conversions are taken from https://docs.oracle.com/javase/specs/jvms/se25/html/jvms-2.html#jvms-2.8
-- Conversion to an integer value uses IEEE 754 roundTowardZero
-- Any other conversion uses IEEE 754 roundTiesToEven
-- Conversions to integer values, IEEE 754 roundTowardsZero:
evalDataNode graph@(nodeInfo -> nodes) (ConvD2I nid) =
  do
    v <- getDouble <$> evalDataNode graph (nodes !!! nid)
    return $ JInt $ fromSDouble sRoundTowardZero v
evalDataNode graph@(nodeInfo -> nodes) (ConvD2L nid) =
  do
    v <- getDouble <$> evalDataNode graph (nodes !!! nid)
    return $ JLong $ fromSDouble sRoundTowardZero v
evalDataNode graph@(nodeInfo -> nodes) (ConvF2I nid) =
  do
    v <- getFloat <$> evalDataNode graph (nodes !!! nid)
    return $ JInt $ fromSFloat sRoundTowardZero v
evalDataNode graph@(nodeInfo -> nodes) (ConvF2L nid) =
  do
    v <- getFloat <$> evalDataNode graph (nodes !!! nid)
    return $ JLong $ fromSFloat sRoundTowardZero v
-- Other conversions, IEEE 754 roundTiesToEven:
evalDataNode graph@(nodeInfo -> nodes) (ConvF2D nid) =
  do
    v <- getFloat <$> evalDataNode graph (nodes !!! nid)
    return $ JDouble $ fromSFloat sRoundNearestTiesToEven v
evalDataNode graph@(nodeInfo -> nodes) (ConvD2F nid) =
  do
    v <- getDouble <$> evalDataNode graph (nodes !!! nid)
    return $ JFloat $ fromSDouble sRoundNearestTiesToEven v
evalDataNode graph@(nodeInfo -> nodes) (ConvI2D nid) =
  do
    v <- getInt <$> evalDataNode graph (nodes !!! nid)
    return $ JDouble $ toSDouble sRoundNearestTiesToEven v
evalDataNode graph@(nodeInfo -> nodes) (ConvI2F nid) =
  do
    v <- getInt <$> evalDataNode graph (nodes !!! nid)
    return $ JFloat $ toSFloat sRoundNearestTiesToEven v
evalDataNode graph@(nodeInfo -> nodes) (ConvL2D nid) =
  do
    v <- getLong <$> evalDataNode graph (nodes !!! nid)
    return $ JDouble $ toSDouble sRoundNearestTiesToEven v
evalDataNode graph@(nodeInfo -> nodes) (ConvL2F nid) =
  do
    v <- getLong <$> evalDataNode graph (nodes !!! nid)
    return $ JFloat $ toSFloat sRoundNearestTiesToEven v
-- Conversion between integer and long https://docs.oracle.com/javase/specs/jvms/se25/html/jvms-2.html#jvms-2.11.4
-- Widening operation, simple sign extension
evalDataNode graph@(nodeInfo -> nodes) (ConvI2L nid) =
  do
    v <- getInt <$> evalDataNode graph (nodes !!! nid)
    return $ JLong $ signExtend32To64 v
  where
    signExtend32To64 :: SInt32 -> SInt64
    signExtend32To64 = fromSized . (signExtend :: SInt 32 -> SInt 64) . toSized
-- Narrowing operation, discard all but the lower 32 bits
evalDataNode graph@(nodeInfo -> nodes) (ConvL2I nid) =
  do
    v <- getLong <$> evalDataNode graph (nodes !!! nid)
    return $ JInt $ dropUpper32 v
  where
    dropUpper32 :: SInt64 -> SInt32
    dropUpper32 = fromSized . (bvExtract (Proxy @31) (Proxy @0) :: SInt 64 -> SInt 32) . toSized
evalDataNode graph (LoadI _ memId addrId) = handleLoad graph JINT memId addrId
evalDataNode graph (LoadL _ memId addrId) = handleLoad graph JLONG memId addrId
evalDataNode graph (LoadF _ memId addrId) = handleLoad graph JFLOAT memId addrId
evalDataNode graph (LoadD _ memId addrId) = handleLoad graph JDOUBLE memId addrId
evalDataNode graph@(nodeInfo -> nodes) node@(LoadP _ objStatus _ctrl memId addrId) =
  -- LoadP is a special case.
  -- We do not support object allocation, assignment nor any other modification.
  -- As such, we always load an object from unknown memory
  do
    address <- evalDataNode graph (nodes !!! addrId)
    unless (objStatus == BotPTR) (raiseException $ "evalDataNode: " <> show node <> " did not load BotPTR")
    unless (nodes !!! memId == ParmMem) (raiseException $ "evalDataNode: " <> show node <> " did not read mem from parmmem")
    return $ Load address
-- NOTE: CastPP casts a pointer from BotPTR to NotNull, i.e.
-- ensures that the object it points to is not null.
-- However, since these are removed and the object status is not updated in the
-- graph after optimizations, it will here be seen as an identity function.
-- TODO: Potentially add path conditions to remedy this
evalDataNode graph@(nodeInfo -> nodes) (CastPP addrId) = evalDataNode graph (nodes !!! addrId)
evalDataNode graph@(nodeInfo -> nodes) (AddP addrId offsetId) =
  do
    address <- evalDataNode graph (nodes !!! addrId)
    case (nodes !!! offsetId) of
      ConL offset -> return $ Offset offset address
      offsetNode -> raiseException $ "AddP: Expected offset to be ConL, got " <> show offsetNode
evalDataNode _ n = raiseException $ "Not a data node:" <> show n

handleLoad :: Graph -> JType -> NodeId -> NodeId -> Symbolic SValue
handleLoad graph@(nodeInfo -> nodes) jtype memId addrId =
  do
    address <- evalDataNode graph (nodes !!! addrId)
    evalMemoryNode graph address jtype memId

handleCMove :: Graph -> NodeId -> NodeId -> Symbolic SValue
handleCMove graph@(nodeInfo -> nodes) n1 n2 =
  do
    let Binary boolGuardId _ = nodes !!! n1
        Binary trueBranch falseBranch = nodes !!! n2
    boolGuard <- ((.== JInt 1)) <$> evalDataNode graph (nodes !!! boolGuardId)
    trueVal <- evalDataNode graph (nodes !!! trueBranch)
    falseVal <- evalDataNode graph (nodes !!! falseBranch)
    return $ ite boolGuard falseVal trueVal

-- | @evalMemoryNode@ will return the possible values that
-- are written to the slice the given memory subgraph
-- is writing to. Much of this is based on the fact
-- that we assume everything is done on slices.
--
-- To illustrate:
-- MemParm 7
--    |
-- StoreI (AddP 12 Object1$A+12) 10
--    |
-- LoadI (AddP 13 Object1$A+12)
--
-- Here, since the AddP nodes used to store and load
-- are different, it is not guaranteed that
-- they write to the same instance.
-- Therefore, @evalMemoryNode@ will return:
-- ite (aliasclass_Object1$A_12 12 == aliasclass_Object1$A 13) 10 (initial_mem 13)
evalMemoryNode ::
  Graph ->
  -- | The address we are looking for
  SValue ->
  -- | The value type we're looking for, mainly to use the correct initial memory
  -- if we cannot find a store that uses the address.
  JType ->
  -- | The memory node's ID
  NodeId ->
  -- | The value read from memory
  Symbolic SValue
evalMemoryNode graph@(nodeInfo -> nodes) address jtype memId =
  case nodes !!! memId of
    ParmMem ->
      -- Base case, we couldn't find any stores that directly wrote to memory,
      -- meaning this memory is unknown.
      case jtype of
        JINT ->
          return . JInt . (_intMem graph) . literal $ showptr address
        JLONG ->
          return . JLong . (_longMem graph) . literal $ showptr address
        JFLOAT ->
          return . JFloat . (_floatMem graph) . literal $ showptr address
        JDOUBLE ->
          return . JDouble . (_doubleMem graph) . literal $ showptr address
    Phi regionId preds ->
      -- Phi nodes may be used by the memory subgraph as well.
      -- The phi node's value depends on the corresponding region node
      let memIdx = (regionPredecessor graph) !!! regionId
          chosenMemId = (preds !! fromIntegral memIdx)
       in evalMemoryNode graph address jtype chosenMemId
    -- All stores are handled with short-circuiting behavior
    (StoreI _ memId' addrId dataId) ->
      unless (jtype == JINT) (raiseException $ "StoreI: Wrong type " <> show jtype)
        >> handleStore memId' addrId dataId
    (StoreL _ memId' addrId dataId) ->
      unless (jtype == JLONG) (raiseException $ "StoreI: Wrong type " <> show jtype)
        >> handleStore memId' addrId dataId
    (StoreF _ memId' addrId dataId) ->
      unless (jtype == JFLOAT) (raiseException $ "StoreI: Wrong type " <> show jtype)
        >> handleStore memId' addrId dataId
    (StoreD _ memId' addrId dataId) ->
      unless (jtype == JDOUBLE) (raiseException $ "StoreI: Wrong type " <> show jtype)
        >> handleStore memId' addrId dataId
    (MergeMem _ _) ->
      -- \| Strong assumption here
      raiseException $ "ASSUMPTION BROKEN: Read from MergeMem"
    node -> raiseException $ "evalMemoryNode: Non-mem node " <> show node
  where
    handleStore memId' addrId dataId =
      do
        address' <- evalDataNode graph (nodes !!! addrId)
        -- \| Short-circuit behavior, if we find that we write to the same address
        -- we read from we can immediately return the value.
        if address' == address
          then evalDataNode graph (nodes !!! dataId)
          else evalMemoryNode graph address jtype memId'

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

runVerification :: SMTConfig -> Graph -> Graph -> IO (Either VerifyException SatResult)
runVerification smtConfig before after =
  try $
    satWith smtConfig $
      do
        parms <- createParams before
        let initialIntMem = (uninterpret $ "intMem" :: SString -> SInt32)
        let initialLongMem = (uninterpret $ "longMem" :: SString -> SInt64)
        let initialFloatMem = (uninterpret $ "floatMem" :: SString -> SFloat)
        let initialDoubleMem = (uninterpret $ "doubleMem" :: SString -> SDouble)
        -- traceM (show before)
        res1 <-
          evalControlNode
            ( before
                { params = parms,
                  _intMem = initialIntMem,
                  _longMem = initialLongMem,
                  _floatMem = initialFloatMem,
                  _doubleMem = initialDoubleMem
                }
            )
            (ParmCtrl 5)
        res2 <-
          evalControlNode
            ( after
                { params = parms,
                  _intMem = initialIntMem,
                  _longMem = initialLongMem,
                  _floatMem = initialFloatMem,
                  _doubleMem = initialDoubleMem
                }
            )
            (ParmCtrl 5)
        -- NOTE: Strong equality, e.g. NaN == NaN but -0 /= +0
        constrain $ res1 ./== res2

-- constrain $ res1 .=== (65, JInt 27)

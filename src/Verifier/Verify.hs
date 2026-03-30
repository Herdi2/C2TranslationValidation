{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Verifier.Verify where

import Control.Monad
import Data.List (findIndex)
import qualified Data.Map as M
import Data.Proxy
import Data.SBV
import Debug.Trace
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
  let [suc] = (controlSuccessors graph) !!! nid
      succNode = (nodeInfo graph) !!! suc
   in evalControlNode
        (updateRegionPredecessors succNode nid graph)
        succNode
evalControlNode graph@(nodeInfo -> nodes) (Return nid dataId) =
  do
    retVal <- evalDataNode graph (nodes !!! dataId)
    return $ (literal nid, retVal)
evalControlNode graph (CallStatic nid) =
  -- NOTE: The return value of a static call is always set to 0, as
  -- we are interested in the node we reach in this case, not the value!
  let retType = methodType graph
   in return $ (literal nid, mkRetValue retType)
evalControlNode graph@(nodeInfo -> nodes) (If nid boolGuardId) =
  do
    let [elseNode, ifNode] =
          -- NOTE: Node Id of the false branch is always greater than the true branch
          case (controlSuccessors graph) !!! nid of
            [a, b] | b > a -> [b, a]
            other -> other
    -- NOTE: Boolean guards require predecessor to be either 0 (false) or 1 (true)
    boolGuard <- ((.== 1) . getInt) <$> evalDataNode graph (nodes !!! boolGuardId)
    ifBranch <- evalControlNode graph $ nodes !!! ifNode
    elseBranch <- evalControlNode graph $ nodes !!! elseNode
    return $
      ite
        boolGuard
        ifBranch
        elseBranch
evalControlNode graph (IfTrue nid) =
  let [suc] = (controlSuccessors graph) !!! nid
      succNode = (nodeInfo graph) !!! suc
   in evalControlNode
        (updateRegionPredecessors succNode nid graph)
        succNode
evalControlNode graph (IfFalse nid) =
  let [suc] = (controlSuccessors graph) !!! nid
      succNode = (nodeInfo graph) !!! suc
   in evalControlNode
        (updateRegionPredecessors succNode nid graph)
        succNode
evalControlNode graph (Region nid _) =
  let [suc] = (controlSuccessors graph) !!! nid
      succNode = (nodeInfo graph) !!! suc
   in evalControlNode
        (updateRegionPredecessors succNode nid graph)
        succNode
evalControlNode _ node = error $ "Not a control node: " <> show node

-- | Symbolically executed the data flow subgraph using denotational semantics
-- NOTE: Very important regarding floating point and doubles.
-- Using e.g. `fpAdd` with explicit roundingmodes to capture the correct semantics
-- of floating point operations generates SMT formulas that take a very long time.
-- I do not see why I shouldn't simply use (+) instead.
-- No false positives yet.
evalDataNode :: Graph -> Node -> Symbolic SValue
evalDataNode (params -> parms) (ParmI var) = return $ parms !!! var
evalDataNode (params -> parms) (ParmL var) = return $ parms !!! var
evalDataNode (params -> parms) (ParmF var) = return $ parms !!! var
evalDataNode (params -> parms) (ParmD var) = return $ parms !!! var
evalDataNode (params -> parms) (ParmMemPtr var _) = return $ parms !!! var
evalDataNode _ (ConI n) = pure $ JInt $ literal n
evalDataNode _ (ConL n) = pure $ JLong $ literal n
evalDataNode _ (ConF n) = pure $ JFloat $ literal n
evalDataNode _ (ConD n) = pure $ JDouble $ literal n
evalDataNode _ (ConP NullPtr) = pure $ NullPtr
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
evalDataNode graph@(nodeInfo -> nodes) (LShiftI n1 n2) =
  -- NOTE: LShift has special semantics in the JVM
  -- int: n1 << (0x1f & n2)
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
evalDataNode graph@(nodeInfo -> nodes) (CmpP n1 n2) =
  do
    v1 <- evalDataNode graph (nodes !!! n1)
    v2 <- evalDataNode graph (nodes !!! n2)
    -- NOTE: When comparing two pointers, only
    -- equality checks (==, !=) are allowed.
    return $ JInt $ ite (v1 .== v2) (literal 0) (literal 1)
evalDataNode graph@(nodeInfo -> nodes) (Bool cmp n) =
  do
    v <- getInt <$> evalDataNode graph (nodes !!! n)
    case cmp of
      Ne -> return $ JInt $ ite (v .== 0) (literal 0) (literal 1)
      Ee -> return $ JInt $ ite (v .== 0) (literal 1) (literal 0)
      Le -> return $ JInt $ ite (v .== -1 .|| v .== 0) (literal 1) (literal 0)
      Lt -> return $ JInt $ ite (v .== -1) (literal 1) (literal 0)
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
evalDataNode graph (LoadI _ memId addrId) = handleLoad graph memId addrId
evalDataNode graph (LoadL _ memId addrId) = handleLoad graph memId addrId
evalDataNode graph (LoadF _ memId addrId) = handleLoad graph memId addrId
evalDataNode graph (LoadD _ memId addrId) = handleLoad graph memId addrId
evalDataNode graph (LoadP _ _ memId addrId) = handleLoad graph memId addrId
evalDataNode _ n = error $ "Not a data node:" <> show n

handleLoad :: Graph -> NodeId -> NodeId -> Symbolic SValue
handleLoad graph@(nodeInfo -> nodes) memId addrId =
  do
    let ptr = readAddress (nodes !!! addrId)
        (JPointer memIndex _ _ _) = ptr
    mem <- evalMemoryNode graph (nodes !!! memId)
    case mem of
      MemSlice memIndex' val ->
        if ptr == memIndex'
          then return val
          else error $ "Load: Mismatching memory slice"
      MemParm ->
        case (classMems graph) M.!? memIndex of
          Nothing -> error $ "Load: couldn't find slice: " <> show memIndex
          Just m -> return m
      MemMerge MemParm aliases -> go aliases
        where
          -- No matching slices, read from bot
          go [] =
            case (classMems graph) M.!? memIndex of
              Nothing -> error $ "Load: couldn't find slice: " <> show memIndex
              Just m -> return m
          -- Check if matching alias exists
          go (MemSlice memIndex' val : xs) =
            if ptr == memIndex'
              then return val
              else go xs
          go _ = error "Load: MergeMem malformed, found non-alias in aliases"
      MemMerge _ _ -> error $ "Load: MergeMem non-parm in bot mem"

evalMemoryNode :: Graph -> Node -> Symbolic Memory
evalMemoryNode graph@(nodeInfo -> nodes) =
  \case
    ParmMem -> return MemParm
    (MergeMem botMemNid aliasNids) ->
      do
        botMem <- evalMemoryNode graph (nodes !!! botMemNid)
        aliases <- sequence $ (\nid -> (evalMemoryNode graph) (nodes !!! nid)) <$> aliasNids
        return $ MemMerge botMem aliases
    (Phi rid preds) ->
      -- Phi nodes may be used by the memory subgraph as well.
      -- The phi node's value depends on the corresponding region node
      let memIdx = (regionPredecessor graph) !!! rid
          chosenMemNode = (nodeInfo graph) !!! (preds !! fromIntegral memIdx)
       in evalMemoryNode graph chosenMemNode
    (StoreI slice memNid addrNid dataNid) -> handleStore slice memNid addrNid dataNid
    (StoreL slice memNid addrNid dataNid) -> handleStore slice memNid addrNid dataNid
    (StoreF slice memNid addrNid dataNid) -> handleStore slice memNid addrNid dataNid
    (StoreD slice memNid addrNid dataNid) -> handleStore slice memNid addrNid dataNid
    (StoreP slice memNid addrNid dataNid) -> handleStore slice memNid addrNid dataNid
    node -> error $ "evalMemoryNode: non-memory node " <> show node
  where
    handleStore slice memNid addrNid dataNid =
      -- For a Store node the slice determines the return value.
      -- If we return a slice, then the value of that slice is
      -- single-handedly determined by the current store, and
      -- previous memory may be ignored.
      -- If we return Bot, then the store node will simply write to the
      -- memory returned by the predecessor, and propagate it forward.
      -- This then forces the predecessing memory to return Bot.
      do
        let memIndex = readAddress (nodes !!! addrNid)
        val <- evalDataNode graph (nodes !!! dataNid)
        mem <- evalMemoryNode graph (nodes !!! memNid)
        case (slice, mem) of
          (Slice _, _) ->
            return $ MemSlice memIndex val
          (Bot, _) -> error $ "ASSUMPTION BROKEN: Store node returned Bot"

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
        ParmMemPtr nid (JPointer memIndex ptrRefinement objectStatus Nothing) ->
          do
            param <- JPointer memIndex ptrRefinement objectStatus <$> (Just <$> (sBool ("parm" <> show nid)))
            go (M.insert nid param paramMap) rest
        ParmMemPtr _ (JPointer _ _ _ (Just _)) ->
          error $ "createParams: JPointer value should not be set"
        _ -> go paramMap rest

-- | Allocate the slices that may potentially be used.
-- This is done by adding a slice that corresponds to every address a Load node may access.
-- Importantly, we only create this "unknown" value for loads that read from unknown memory.
-- For now, this is only in the case of a memparm.
--
-- NOTE: It would be nicer to do this using a state monad, but allocating SMT variables to use
-- before using them seems to be the correct approach (https://github.com/LeventErkok/sbv/issues/613)
createMemory :: Graph -> Symbolic (M.Map MemIndex SValue)
createMemory (nodeInfo -> nodes) = go M.empty (map snd $ M.toList nodes)
  where
    go slices [] = return slices
    go slices (x : xs) =
      case x of
        (LoadI source memNid addrNid) ->
          addSlice source slices addrNid JINT >>= flip go xs
        (LoadL source memNid addrNid) ->
          addSlice source slices addrNid JLONG >>= flip go xs
        (LoadF source memNid addrNid) ->
          addSlice source slices addrNid JFLOAT >>= flip go xs
        (LoadD source memNid addrNid) ->
          addSlice source slices addrNid JDOUBLE >>= flip go xs
        (LoadP source pointerVal memNid addrNid) ->
          addPtrSlice source slices addrNid pointerVal >>= flip go xs
        _ -> go slices xs
    addSlice source slices nid jtype =
      let JPointer memIndex _ _ Nothing = readAddress (nodes !!! nid)
       in if (memIndex `M.member` slices)
            then return slices
            else do
              sliceVal <-
                ( case jtype of
                    JINT -> JInt <$> sInt32 source
                    JLONG -> JLong <$> sInt64 source
                    JFLOAT -> JFloat <$> sFloat source
                    JDOUBLE -> JDouble <$> sDouble source
                )
              return $ M.insert memIndex sliceVal slices
    addPtrSlice source slices nid (JPointer (className, offset) ptrRefinement objStatus Nothing) =
      let JPointer memIndex _ _ Nothing = readAddress (nodes !!! nid)
       in if (memIndex `M.member` slices)
            then return slices
            else do
              let createPtr = JPointer (className, offset) ptrRefinement objStatus . Just
              botPtr <- sBool source
              let sliceVal =
                    case objStatus of
                      Null -> createPtr $ fromBool True
                      NotNull -> createPtr $ fromBool False
                      BotPTR -> createPtr $ botPtr
              return $ M.insert memIndex sliceVal slices

-- | Creates an uninterpreter function that keeps track of memory aliasing
-- Unless the load and store nodes read the relevant address from the
-- same AddP node, the instances of the objects they read may not be the same.
-- However, if the AddP nodes are the same, then the two objects are guaranteed
-- to be the same instance.
--
-- To illustrate:
-- MemParm 7
--    |
-- StoreI (AddP 12 Object1$A+12) 10
--    |
-- LoadI (AddP 13 Object1$A+12)
--
-- Now, the value of the load is not solely determined by
-- the StoreI above it due to using a different AddP node.
-- This means that the instances of Object1$A used in the store
-- and in the load may differ.
-- Therefore, the loaded value may be 10 (as given by the StoreI)
-- or it may be unknown, given by MemParm.
--
-- To model this, we create an uninterpreted function
-- `alias_class_object1$A_12 :: SInteger -> SInteger`.
-- This function will map each AddP id to an alias Id.
-- In the case above, e.g.
-- AddP 12 Object1$A+12 gets assigned id x
-- AddP 13 Object1$A+12 gets assigned id y.
-- Now, x and y may be equal, which means they alias, or they may be different!
-- Note, these functions are the same for both before and after graphs,
-- to make sure that the same aliasing set-up is used.
createAliasClass :: Graph -> M.Map MemIndex (SWord64 -> SWord64)
createAliasClass (nodeInfo -> nodes) = go M.empty (M.toList nodes)
  where
    go aliasClasses [] = aliasClasses
    go aliasClasses ((nid, node) : rest) =
      case node of
        (LoadI _ memId addrId) -> undefined
        (LoadL _ memId addrId) -> undefined
        (LoadF _ memId addrId) -> undefined
        (LoadD _ memId addrId) -> undefined
        (LoadP _ _ memId addrId) -> undefined
        _ -> go aliasClasses rest
    createFunc aliasClasses memId =
      let ptr = readAddress (nodes !!! memId)
          (JPointer memIndex@(className, offset) _ _ _) = ptr
       in M.insert
            memIndex
            ( uninterpret $ "aliasclass_" <> className <> "_" <> show offset ::
                SWord64 -> SWord64
            )
            aliasClasses

-- | Creates a symbolic array for each memory parameter,
-- which represents the memory of a given class.
runVerification :: SMTConfig -> Graph -> Graph -> IO SatResult
runVerification smtConfig before after =
  satWith smtConfig $
    do
      parms <- createParams before
      mems <- createMemory before
      let aliasfuncs = createAliasClass before
      res1 <-
        evalControlNode
          ( before
              { params = parms,
                classMems = mems,
                aliasClasses = aliasfuncs
              }
          )
          (ParmCtrl 5)
      res2 <-
        evalControlNode
          ( after
              { params = parms,
                classMems = mems,
                aliasClasses = aliasfuncs
              }
          )
          (ParmCtrl 5)
      -- NOTE: Strong equality, e.g. NaN == NaN but -0 /= +0
      -- constrain $ res1 .=== (35, JInt $ 20)
      constrain $ res1 ./== res2

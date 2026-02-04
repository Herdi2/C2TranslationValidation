{-# LANGUAGE DataKinds #-}
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
import GHC.Natural

type NodeId = Natural

type ParamMap = M.Map NodeId SValue

type NodeInfo = M.Map NodeId Node

data JKind
  = IntKind
  | LongKind
  | FloatKind
  | DoubleKind
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
  | -- Addition nodes
    Add JKind NodeId NodeId
  | -- | Subtraction ndoes
    Sub JKind NodeId NodeId
  | -- | Multiplication nodes
    Mul JKind NodeId NodeId
  | MulHiL NodeId NodeId
  | -- | Division nodes
    Div JKind NodeId NodeId
  | -- | Bitwise and
    BitAnd JKind NodeId NodeId
  | -- | Bitwise or
    BitOr JKind NodeId NodeId
  | -- | Bitshifting
    LShift JKind NodeId NodeId
  | RShift JKind NodeId NodeId
  | -- | Starting point of the control flow subgraph
    ParmCtrl NodeId
  | -- | Phi with id and predecessor ids
    Phi
      NodeId
      [NodeId]
  | -- | Region node with list of predecessors
    Region [NodeId]
  | -- | If node with control flow predecessor and boolean expr
    If NodeId NodeId
  | -- | IfTrue projection with predecessor
    IfT NodeId
  | -- | IfTrue projection with predecessor
    IfF NodeId
  | -- | Return with data flow predecessor
    Return NodeId
  deriving (Show, Eq)

data Graph
  = Graph
  { nodeInfo :: M.Map NodeId Node,
    controlSuccessors :: M.Map NodeId [NodeId],
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

-- | (side effect, return value)
-- e.g. (0, 64) is normal return with value 64
-- Any exception node will instead have their nodeId in the tuple, e.g. (11, ...)
-- for exception on node 11.
type Ret = (SWord32, SValue)

-- | Symbolically executes the control flow subgraph using operational semantics
evalControlNode :: Graph -> Node -> Symbolic Ret
evalControlNode graph (ParmCtrl nid) =
  do
    let [succ] = (controlSuccessors graph) M.! nid
    evalControlNode graph $ (nodeInfo graph) M.! succ
evalControlNode graph@(nodeInfo -> nodes) (Return predId) =
  do
    retVal <- evalDataNode graph (nodes M.! predId)
    return $ (literal 0, retVal)
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
evalDataNode graph@(nodeInfo -> nodes) (Add jkind n1 n2) =
  do
    v1 <- evalDataNode graph (nodes M.! n1)
    v2 <- evalDataNode graph (nodes M.! n2)
    case jkind of
      IntKind -> return $ JInt (getInt v1 + getInt v2)
      LongKind -> return $ JLong (getLong v1 + getLong v2)
      FloatKind -> return $ JFloat (getFloat v1 + getFloat v2)
      DoubleKind -> return $ JDouble (getDouble v1 + getDouble v2)
evalDataNode graph@(nodeInfo -> nodes) (Sub jkind n1 n2) =
  do
    v1 <- evalDataNode graph (nodes M.! n1)
    v2 <- evalDataNode graph (nodes M.! n2)
    case jkind of
      IntKind -> return $ JInt (getInt v1 - getInt v2)
      LongKind -> return $ JLong (getLong v1 - getLong v2)
      FloatKind -> return $ JFloat (getFloat v1 - getFloat v2)
      DoubleKind -> return $ JDouble (getDouble v1 - getDouble v2)
evalDataNode graph@(nodeInfo -> nodes) (Mul jkind n1 n2) =
  do
    v1 <- evalDataNode graph (nodes M.! n1)
    v2 <- evalDataNode graph (nodes M.! n2)
    case jkind of
      IntKind -> return $ JInt (getInt v1 * getInt v2)
      LongKind -> return $ JLong (getLong v1 * getLong v2)
      FloatKind -> return $ JFloat (getFloat v1 * getFloat v2)
      DoubleKind -> return $ JDouble (getDouble v1 * getDouble v2)
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
evalDataNode graph@(nodeInfo -> nodes) (Div jkind n1 n2) =
  do
    v1 <- evalDataNode graph (nodes M.! n1)
    v2 <- evalDataNode graph (nodes M.! n2)
    case jkind of
      IntKind -> return $ JInt (getInt v1 `sDiv` getInt v2)
      LongKind -> return $ JLong (getLong v1 `sDiv` getLong v2)
      FloatKind -> return $ JFloat (getFloat v1 / getFloat v2)
      DoubleKind -> return $ JDouble (getDouble v1 / getDouble v2)
evalDataNode graph@(nodeInfo -> nodes) (BitAnd jkind n1 n2) =
  do
    v1 <- evalDataNode graph (nodes M.! n1)
    v2 <- evalDataNode graph (nodes M.! n2)
    case jkind of
      IntKind -> return $ JInt (getInt v1 .&. getInt v2)
      LongKind -> return $ JLong (getLong v1 .&. getLong v2)
      _ -> error $ "Invalid bitwise AND with kind " <> show jkind
evalDataNode graph@(nodeInfo -> nodes) (BitOr jkind n1 n2) =
  do
    v1 <- evalDataNode graph (nodes M.! n1)
    v2 <- evalDataNode graph (nodes M.! n2)
    case jkind of
      IntKind -> return $ JInt (getInt v1 .|. getInt v2)
      LongKind -> return $ JLong (getLong v1 .|. getLong v2)
      _ -> error $ "Invalid bitwise OR with kind " <> show jkind
evalDataNode graph@(nodeInfo -> nodes) (LShift jkind n1 n2) =
  -- NOTE: LShift has special semantics in the JVM
  -- int: n1 << (0x1f & n2)
  -- long: n1 << (0x3f & n2)
  do
    v1 <- evalDataNode graph (nodes M.! n1)
    v2 <- getInt <$> evalDataNode graph (nodes M.! n2)
    case jkind of
      IntKind -> return $ JInt (getInt v1 `sShiftLeft` (literal 0x1f .&. v2))
      LongKind -> return $ JLong (getLong v1 `sShiftLeft` (literal 0x3f .&. v2))
      _ -> error $ "Invalid LSHIFT with kind " <> show jkind
evalDataNode graph@(nodeInfo -> nodes) (RShift jkind n1 n2) =
  -- NOTE: Rshift has special semantics in the JVM
  -- int: n1 >> (0x1f & n2)
  -- long: n1 >> (0x3f & n2)
  do
    v1 <- evalDataNode graph (nodes M.! n1)
    v2 <- getInt <$> evalDataNode graph (nodes M.! n2)
    case jkind of
      IntKind -> return $ JInt (getInt v1 `sShiftRight` (literal 0x1f .&. v2))
      LongKind -> return $ JLong (getLong v1 `sShiftRight` (literal 0x3f .&. v2))
      _ -> error $ "Invalid RSHIFT with kind " <> show jkind
evalDataNode _ n = error $ "Not a data node:" <> show n

mkGraph :: NodeInfo -> Graph
mkGraph nInfo = Graph {nodeInfo = nInfo, params = M.empty}

testGraph :: Graph
testGraph =
  mkGraph $
    M.fromList [(1, ConI 14), (2, ConI 15), (3, Add IntKind 1 2), (4, ParmI 4)]

binopGraph :: Graph
binopGraph =
  mkGraph $
    M.fromList
      [ (29, Return 28),
        (28, Add IntKind 25 27),
        (25, Mul IntKind 10 11),
        (27, Add IntKind 10 10),
        (10, ParmI 10),
        (11, ParmI 11)
      ]

andNegBefore :: Graph
andNegBefore =
  mkGraph $
    M.fromList
      [ (10, ParmI 10),
        (11, ParmI 11),
        (23, ConI 0),
        (24, Sub IntKind 23 10),
        (25, Sub IntKind 23 11),
        (26, BitAnd IntKind 24 25),
        (27, Return 26)
      ]

andNegAfter :: Graph
andNegAfter =
  mkGraph $
    M.fromList
      [ (10, ParmI 10),
        (11, ParmI 11),
        (26, BitAnd IntKind 10 11),
        (27, Return 26)
      ]

lshiftBefore :: Graph
lshiftBefore =
  mkGraph $
    M.fromList
      [ (10, ParmL 10),
        (23, Add LongKind 10 10),
        (24, ConI 63),
        (25, LShift LongKind 23 24),
        (26, Return 25)
      ]

lshiftAfter :: Graph
lshiftAfter =
  mkGraph $
    M.fromList
      [ (10, ParmL 10),
        (26, Return 10)
      ]

mulassoBefore :: Graph
mulassoBefore =
  mkGraph $
    M.fromList
      [ (10, ParmF 10),
        (22, ConF 00111101111110111110011101101101),
        (23, ConF 00111111100111010111000010100100),
        (24, Mul FloatKind 10 22),
        (25, Mul FloatKind 24 23),
        (26, Return 25)
      ]

mulassoAfter :: Graph
mulassoAfter =
  mkGraph $
    M.fromList
      [ (10, ParmF 10),
        (27, ConF 00111110000110101110101111000100),
        (25, Mul FloatKind 10 27),
        (26, Return 25)
      ]

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
    let before = mulassoBefore
    let after = mulassoAfter
    res <- satWith z3 {verbose = True, timing = PrintTiming} $
      do
        params <- createParams before
        res1 <- evalControlNode (before {params = params}) (Return 25)
        res2 <- evalControlNode (after {params = params}) (Return 25)
        -- NOTE: Strong equality, e.g. NaN == NaN but -0 /= +0
        constrain $ res1 ./== res2
    print res

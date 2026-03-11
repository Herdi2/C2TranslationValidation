{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- - Module: Graph
-- - Exports RawGraph (Used in XML parsing in `GraphParser`) and Graph (SoN graph, used in `Verify` and `GraphBuilder`)
module Verifier.Graph where

import Data.List (mapAccumL)
import qualified Data.Map as M
import Data.Proxy
import Data.SBV
import Debug.Trace
import GHC.Prim

type NodeId = Word32

type ParamMap = M.Map NodeId SValue

type NodeInfo = M.Map NodeId Node

data ControlSucc
  = Next NodeId
  | IfNext NodeId NodeId
  deriving (Show, Eq)

type ControlSuccessors = M.Map NodeId [NodeId]

data Comp
  = -- | Not Equal
    Ne
  | -- | Less than or equal
    Le
  | -- | Less than
    Lt
  deriving (Show, Eq)

data Node
  = -- | Parameters carry their own nodeId for parameter handling (see @createParams@)
    ParmI NodeId
  | ParmL NodeId
  | ParmF NodeId
  | ParmD NodeId
  | -- | ParmMem represents the initial memory of the method
    ParmMem
  | -- | ParmMemPtr contains the initial blockId and offset of the memory
    -- Usually, offset=0
    ParmMemPtr String
  | -- | Constant nodes
    ConI Int32
  | ConL Int64
  | ConF Float
  | ConD Double
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
    ConvD2F NodeId
  | ConvD2I NodeId
  | ConvD2L NodeId
  | ConvF2D NodeId
  | ConvF2I NodeId
  | ConvF2L NodeId
  | ConvI2D NodeId
  | ConvI2F NodeId
  | ConvI2L NodeId
  | ConvL2D NodeId
  | ConvL2F NodeId
  | ConvL2I NodeId
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
  | -- | IfTrue projection
    IfFalse NodeId
  | -- | Return <nodeId> <dataflow predecessor node id>
    Return NodeId NodeId
  | -- | Static calls with own Id
    CallStatic NodeId
  | -- | Store <Slice/Alias> <Mem id> <address> <value>
    StoreI MemLattice NodeId NodeId NodeId
  | StoreL MemLattice NodeId NodeId NodeId
  | StoreF MemLattice NodeId NodeId NodeId
  | StoreD MemLattice NodeId NodeId NodeId
  | -- | Load <Mem id> <address>
    LoadI NodeId NodeId
  | LoadL NodeId NodeId
  | LoadF NodeId NodeId
  | LoadD NodeId NodeId
  | -- | MergeMem <Bot memory> [<Alias memory>]
    MergeMem NodeId [NodeId]
  | -- | AddP <ptr1> <ptr2> <offset>, ptr1 := ptr2 + offset
    AddP MemIndex NodeId NodeId NodeId
  deriving (Show, Eq)

data RawNode = RawNode
  { rawNodeId :: String,
    rawNodeName :: String,
    rawNodeProps :: M.Map String String
  }
  deriving (Show)

-- | (from, to, index)
type RawEdge = (NodeId, NodeId, NodeId)

data RawGraph = RawGraph
  { rawNodes :: [RawNode],
    rawEdges :: [RawEdge]
  }
  deriving (Show)

data Graph
  = Graph
  { -- | Method return type
    methodType :: JType,
    -- | Maps nodeId (SoN id) to Node
    nodeInfo :: NodeInfo,
    -- | Contains successors for the control flow nodes
    controlSuccessors :: ControlSuccessors,
    -- | Keeps track of which predecessor was last used to enter Region node.
    -- Used to determine value given by Phi nodes.
    regionPredecessor :: M.Map NodeId NodeId,
    -- | Contains unified variables for parameters.
    -- Both to be reused within the graph and unify between the graphs.
    params :: ParamMap,
    -- | Contains mapping from string to SMT array.
    -- Used to represent alias/memory of a given class
    classMems :: M.Map String ClassMemory
  }
  deriving (Show, Eq)

-- | Graph with default values (everything empty)
-- NOTE: The default return type is `int`, since we always want to have a return statement
defaultGraph :: Graph
defaultGraph = Graph JINT M.empty M.empty M.empty M.empty M.empty

mkGraph :: JType -> [(NodeId, Node)] -> [(NodeId, [NodeId])] -> Graph
mkGraph retType nInfo successors =
  Graph
    { methodType = retType,
      nodeInfo = M.fromList nInfo,
      controlSuccessors = M.fromList successors,
      regionPredecessor = M.empty,
      params = M.empty,
      classMems = M.empty
    }

-- | Used to define and keep track of valid method return types
data JType
  = JINT
  | JLONG
  | JFLOAT
  | JDOUBLE
  deriving (Show, Eq)

mkRetValue :: JType -> SValue
mkRetValue JINT = JInt 0
mkRetValue JLONG = JLong 0
mkRetValue JFLOAT = JFloat 0
mkRetValue JDOUBLE = JDouble 0

-- | Symbolic values, showing equivalence between Java values and SMT values
data SValue
  = JInt SInt32
  | JLong SInt64
  | -- | FP 8 24
    JFloat SFloat
  | -- | FP 11 53
    JDouble SDouble
  deriving (Show, Eq)

instance EqSymbolic SValue where
  (.==) a b =
    case (a, b) of
      (JInt v1, JInt v2) -> v1 .== v2
      (JLong v1, JLong v2) -> v1 .== v2
      (JFloat v1, JFloat v2) -> v1 .== v2
      (JDouble v1, JDouble v2) -> v1 .== v2
      (_, _) -> sFalse

  -- NOTE: Important to also implement (.===) to get equality between NaNs
  -- Otherwise we get many false-positives.
  (.===) a b =
    case (a, b) of
      (JInt v1, JInt v2) -> v1 .=== v2
      (JLong v1, JLong v2) -> v1 .=== v2
      (JFloat v1, JFloat v2) -> v1 .=== v2
      (JDouble v1, JDouble v2) -> v1 .=== v2
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

{- MEMORY REPRESENTATION -}
-- (BlockId, Offset)
type MemIndex = (String, Word64)

-- | Models the memory lattice, which is Bot (Whole memory) or a slice (one field/memory address)
data MemLattice = Bot | Slice Integer deriving (Show, Eq)

-- | Each class has memory represented by an array of bytes,
-- which stores the values of each field.
type ClassMemory = SArray Word64 Word8

-- | @Memory@ represents our three different memory representations,
-- which are the three different values the memory subgraph may return
data Memory
  = -- | A memory slice, which represents one index (one field/mem position)
    MemAlias Integer MemIndex SValue
  | -- | Bot, i.e. the whole memory.
    -- This is represented as an SMT array
    -- Importantly, this will mostly be used for when we do not know the value we wish to return.
    MemBot ClassMemory
  | -- | Memory parameter, base case for traversing the memory graph
    MemParm
  | -- | MergeMem, which contains both Bot (first slice) and any aliases that are being unioned with.
    MemMerge Memory [Memory]

-- Write to array representing class memory
-- NOTE: Since fields have different types, the memory of a class is a byte array.
-- Values of a field, e.g. an integer, will be written using contiguous fields
-- e.g
-- Writing int @ index 12
-- 12: Int(31, 24)
-- 13: Int(23, 16)
-- 14: Int(15, 8)
-- 15: Int(7, 0)
writeMem :: Symbolic ClassMemory -> Word64 -> SValue -> Symbolic ClassMemory
writeMem classMem key val =
  do
    evaluatedMem <- classMem
    return $
      case val of
        (JInt v) -> go evaluatedMem (toBytes (sFromIntegral v :: SWord 32))
        (JLong v) -> go evaluatedMem (toBytes (sFromIntegral v :: SWord 64))
        (JFloat v) -> go evaluatedMem (toBytes (toSized $ sFloatAsSWord32 v))
        (JDouble v) -> go evaluatedMem (toBytes (toSized $ sDoubleAsSWord64 v))
  where
    go :: ClassMemory -> [SWord 8] -> ClassMemory
    go mem val = fst $ mapAccumL (\m b -> (writeArray m (literal key) b, b + 1)) mem (fmap fromSized val)

readMem :: Symbolic ClassMemory -> SWord64 -> JType -> Symbolic SValue
readMem classMem key jtyp =
  do
    evaluatedMem <- classMem
    return $
      case jtyp of
        JINT -> JInt (sFromIntegral (fromBytes (go evaluatedMem key 4) :: SWord 32))
        JLONG -> JLong (sFromIntegral (fromBytes (go evaluatedMem key 8) :: SWord 64))
        JFLOAT -> JFloat (sWord32AsSFloat $ fromSized $ (fromBytes (go evaluatedMem key 4) :: SWord 32))
        JDOUBLE -> JDouble (sWord64AsSDouble $ fromSized $ (fromBytes (go evaluatedMem key 8) :: SWord 64))
  where
    go :: ClassMemory -> SWord64 -> Int -> [SWord 8]
    go mem key byteCount = map toSized $ readArray mem <$> (take byteCount $ iterate (+ 1) key)

readAddress :: Node -> MemIndex
readAddress (AddP m _ _ _) = m
readAddress n = error $ "Store node got address from node other than AddP: " <> show n

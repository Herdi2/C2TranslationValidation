{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- - Module: Graph
-- - Exports RawGraph (Used in XML parsing in `GraphParser`) and Graph (SoN graph, used in `Verify` and `GraphBuilder`)
module Verifier.Graph where

import Data.List (mapAccumL)
import qualified Data.Map as M
import Data.Proxy
import Data.SBV
import qualified Data.SBV.Internals as SI
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
  | -- | Equal
    Ee
  deriving (Show, Eq)

data Node
  = -- | Parameters carry their own nodeId for parameter handling (see @createParams@)
    ParmI NodeId
  | ParmL NodeId
  | ParmF NodeId
  | ParmD NodeId
  | -- | ParmMemPtr contains pointer to own object, i.e. "this"
    ParmMemPtr NodeId SValue
  | -- | ParmMem represents the initial memory of the method
    ParmMem
  | -- | Constant nodes
    ConI Int32
  | ConL Int64
  | ConF Float
  | ConD Double
  | ConP SValue
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
  | CmpP NodeId NodeId
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
  | StoreP MemLattice NodeId NodeId NodeId
  | -- | Load <field name> <Mem id> <address>
    LoadI String NodeId NodeId
  | LoadL String NodeId NodeId
  | LoadF String NodeId NodeId
  | LoadD String NodeId NodeId
  | LoadP String SValue NodeId NodeId
  | -- | MergeMem <Bot memory> [<Alias memory>]
    MergeMem NodeId [NodeId]
  | -- | AddP <ptr result> <ptr1> <ptr2> <offset>
    AddP SValue NodeId NodeId NodeId
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
    -- | Contains mapping from a slice to the corresponding SMT value
    classMems :: M.Map MemIndex SValue,
    -- | AliasClasses. See @createAliasClass@ for documentation.
    aliasClasses :: M.Map MemIndex (SWord64 -> SWord64)
  }

-- | Graph with default values (everything empty)
-- NOTE: The default return type is `int`, since we always want to have a return statement
defaultGraph :: Graph
defaultGraph = Graph JINT M.empty M.empty M.empty M.empty M.empty M.empty

mkGraph :: JType -> [(NodeId, Node)] -> [(NodeId, [NodeId])] -> Graph
mkGraph retType nInfo successors =
  Graph
    { methodType = retType,
      nodeInfo = M.fromList nInfo,
      controlSuccessors = M.fromList successors,
      regionPredecessor = M.empty,
      params = M.empty,
      classMems = M.empty,
      aliasClasses = M.empty
    }

-- | Used to define and keep track of valid method return types
data JType
  = JINT
  | JLONG
  | JFLOAT
  | JDOUBLE
  | JPOINTER
  deriving (Show, Eq)

mkRetValue :: JType -> SValue
mkRetValue JINT = JInt 0
mkRetValue JLONG = JLong 0
mkRetValue JFLOAT = JFloat 0
mkRetValue JDOUBLE = JDouble 0

type MemIndex = (String, Word64)

-- | Symbolic values, showing equivalence between Java values and SMT values
data SValue
  = JInt SInt32
  | JLong SInt64
  | -- | FP 8 24
    JFloat SFloat
  | -- | FP 11 53
    JDouble SDouble
  | -- | Object pointers
    -- We wish to know which class is its base, the offset into the class,
    -- refinement level and whether it is Null (True) or NotNull (False).
    -- BotPTR can be either Null or NotNull, which can be modeled using a symbolic value SBool
    JPointer MemIndex PtrRefinement ObjectStatus (Maybe SBool)
  | -- | Represent null pointers
    NullPtr
  deriving (Show, Eq)

instance EqSymbolic SValue where
  (.==) a b =
    case (a, b) of
      (JInt v1, JInt v2) -> v1 .== v2
      (JLong v1, JLong v2) -> v1 .== v2
      (JFloat v1, JFloat v2) -> v1 .== v2
      (JDouble v1, JDouble v2) -> v1 .== v2
      (JPointer memIdx1 ptrRef1 objStat1 x, JPointer memIdx2 ptrRef2 objStat2 y) ->
        fromBool (memIdx1 == memIdx2 && ptrRefEq ptrRef1 ptrRef2 && objStat1 == objStat2)
          .&& x .== y
      -- NOTE: The boolean in a JPointer tells us whether it is a nullptr or not
      (JPointer _ _ _ x, NullPtr) ->
        x .== Just (fromBool True)
      (NullPtr, JPointer _ _ _ x) ->
        x .== Just (fromBool True)
      (_, _) -> sFalse

  -- NOTE: Important to also implement (.===) to get equality between NaNs
  -- Otherwise we get many false-positives.
  (.===) a b =
    case (a, b) of
      (JInt v1, JInt v2) -> v1 .=== v2
      (JLong v1, JLong v2) -> v1 .=== v2
      (JFloat v1, JFloat v2) -> v1 .=== v2
      (JDouble v1, JDouble v2) -> v1 .=== v2
      (JPointer memIdx1 ptrRef1 objStat1 x, JPointer memIdx2 ptrRef2 objStat2 y) ->
        fromBool (memIdx1 == memIdx2 && ptrRefEq ptrRef1 ptrRef2 && objStat1 == objStat2)
          .&& x .=== y
      (JPointer memIdx1 ptrRef1 objStat1 x, NullPtr) ->
        x .=== Nothing
      (NullPtr, JPointer memIdx1 ptrRef1 objStat1 x) ->
        x .=== Nothing
      (_, _) -> error $ "Tried to compare different SValue types"

instance Mergeable SValue where
  symbolicMerge force test left right = case (left, right) of
    (JInt x, JInt y) -> JInt $ symbolicMerge force test x y
    (JLong x, JLong y) -> JLong $ symbolicMerge force test x y
    (JFloat x, JFloat y) -> JFloat $ symbolicMerge force test x y
    (JDouble x, JDouble y) -> JDouble $ symbolicMerge force test x y
    (JPointer memidx ref objstatus x, JPointer _ _ _ y) ->
      JPointer memidx ref objstatus $ symbolicMerge force test x y
    (x, y) -> error $ "Cannot merge different SValue types: " <> show x <> " and " <> show y

instance OrdSymbolic SValue where
  (.<) a b =
    case (a, b) of
      (JInt x, JInt y) -> x .< y
      (JLong x, JLong y) -> x .< y
      (JFloat x, JFloat y) -> x .< y
      (JDouble x, JDouble y) -> x .< y
      _ -> error "OrdSymbolic: Cannot compare different SValue types"

{- MEMORY REPRESENTATION -}

-- | Refinement level of pointers
data PtrRefinement = InstPtr | Ptr deriving (Show, Eq)

ptrRefEq :: PtrRefinement -> PtrRefinement -> Bool
ptrRefEq p1 p2 = p1 == p2

-- | Status of the object pointer to, either Null, NotNull or BotPTR (unknown if null or not)
data ObjectStatus = Null | NotNull | BotPTR deriving (Show, Eq)

-- | Models the memory lattice, which is Bot (Whole memory) or a slice (one field/memory address)
data MemLattice = Bot | Slice Integer deriving (Show, Eq)

-- | In our assumptions about the memory subgraph, we have three different types of memory we read from:
-- 1. A slice, which is what we assume every store node creates
-- 2. The memory parameter - initial unknown memory of the method, which we do not know the values of.
-- 3. A MergeMem node, which contains slices and a representation of the Bot memory.
-- Note that, due to our assumption that Store nodes cannot create Bot, the Bot memory of the
-- MergeMem node *has* to be provided by the memory parameter
data Memory
  = -- Pointer value, value
    MemSlice SValue SValue
  | MemParm
  | MemMerge Memory [Memory]
  deriving (Eq, Show)

readAddress :: Node -> SValue
readAddress (AddP m _ _ _) = m
readAddress n = error $ "Store node got address from node other than AddP: " <> show n

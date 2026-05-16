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
import Prettyprinter

type NodeId = Word32

type SNodeId = SWord32

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
  | -- | Greater than
    Gt
  | -- | Greater than or equal
    Ge
  | -- | Equal
    Ee
  deriving (Show, Read, Eq)

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
  | ConP String
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
  | -- | Bitwise xor
    XorI NodeId NodeId
  | XorL NodeId NodeId
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
  | -- Cast nodes, semantically equivalent to constant nodes
    CastII SValue
  | -- | Comparisons
    CmpI NodeId NodeId
  | CmpL NodeId NodeId
  | CmpF NodeId NodeId
  | CmpD NodeId NodeId
  | CmpP NodeId NodeId
  | CmpU NodeId NodeId
  | CmpUL NodeId NodeId
  | -- | Move nodes
    -- CMoveX <Binary node (Bool)> <Binary node (Branches)>
    CMoveI NodeId NodeId
  | CMoveL NodeId NodeId
  | CMoveF NodeId NodeId
  | CMoveD NodeId NodeId
  | Binary NodeId NodeId
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
  | -- | Rethrow <nodeId>
    Rethrow NodeId
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
  | -- LoadP is a special case
    -- LoadP is in our implementation only used for comparisons, into CmpP.
    -- As such, we need to keep track of ObjectStatus (maybe it is already guaranteed to be non-null or null)
    -- Memory we load from (should always be the initial mem since we do support object assignments)
    -- and the offset as usual
    -- We also keep track of the control flow, if it exists, to assert a path condition
    LoadP String ObjectStatus (Maybe NodeId) NodeId NodeId
  | -- | MergeMem <Bot memory> [<Alias memory>]
    MergeMem NodeId [NodeId]
  | -- | AddP <ptr> <offset>
    AddP NodeId NodeId
  | -- | CastPP <ptr>
    CastPP NodeId
  deriving (Show, Read, Eq)

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
    _aliasClasses :: M.Map MemIndex (SNodeId -> SWord64),
    -- | Initial memory functions. Maps alias indices
    -- from @_aliasClasses@ to free variables.
    _intMem :: SString -> SInt32,
    _longMem :: SString -> SInt64,
    _floatMem :: SString -> SFloat,
    _doubleMem :: SString -> SDouble,
    _pointerMem :: SString -> SBool
  }

instance Eq Graph where
  (==)
    (Graph x1 x2 x3 x4 x5 x6 _ _ _ _ _ _)
    (Graph y1 y2 y3 y4 y5 y6 _ _ _ _ _ _) =
      x1 == y1 && x2 == y2 && x3 == y3 && x4 == y4 && x5 == y5 && x6 == y6

instance Show Graph where
  show (Graph _1 _2 _3 _4 _5 _6 _7 _ _ _ _ _) =
    "Graph: "
      <> ( unlines
             [ show _1,
               show _2,
               show _3,
               show _4,
               show _5,
               show _6,
               "alias_classes" <> show (fst <$> M.toList _7)
             ]
         )

instance Pretty Graph where
  pretty graph =
    let retType = methodType graph
        nodes = M.toList $ nodeInfo graph
        controlFlow = M.toList $ controlSuccessors graph
     in pretty "Return type: "
          <+> pretty (show retType)
          <> line
          <> pretty (map show nodes)
          <> line
          <> pretty (map show controlFlow)

-- | Graph with default values (everything empty)
-- NOTE: The default return type is `int`, since we always want to have a return statement
defaultGraph :: Graph
defaultGraph =
  Graph
    JINT
    M.empty
    M.empty
    M.empty
    M.empty
    M.empty
    M.empty
    (uninterpret $ "initialIntMem" :: SString -> SInt32)
    (uninterpret $ "initialLongMem" :: SString -> SInt64)
    (uninterpret $ "initialFloatMem" :: SString -> SFloat)
    (uninterpret $ "initialDoubleMem" :: SString -> SDouble)
    (uninterpret $ "initialPointerMem" :: SString -> SBool)

mkGraph :: JType -> [(NodeId, Node)] -> [(NodeId, [NodeId])] -> Graph
mkGraph retType nInfo successors =
  defaultGraph
    { methodType = retType,
      nodeInfo = M.fromList nInfo,
      controlSuccessors = M.fromList successors
    }

-- | Used to define and keep track of valid method return types
data JType
  = JINT
  | JLONG
  | JFLOAT
  | JDOUBLE
  | JPOINTER
  | ERROR
  deriving (Show, Read, Eq)

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
    -- | New memory values
    Base String
  | Offset Int64 SValue
  | Load SValue
  deriving (Show, Eq)

showptr :: SValue -> String
showptr (Base className) = className
showptr (Offset offset ptr) = showptr ptr <> "_" <> show offset
showptr (Load ptr) = "$" <> showptr ptr <> "$"
showptr val = error $ "showptr: non-ptr " <> show val

checkptr :: SValue -> Bool
checkptr (Base _) = True
checkptr (Offset _ ptr) = checkptr ptr
checkptr (Load ptr) = checkptr ptr
checkptr _ = False

instance Read SValue where
  readsPrec _ input =
    case words input of
      ("JInt" : jint : rest) -> [(JInt (literal (read jint :: Int32)), unwords rest)]
      ("JLong" : jlong : rest) -> [(JLong (literal (read jlong :: Int64)), unwords rest)]
      ("JFloat" : jfloat : rest) -> [(JFloat (literal (read jfloat :: Float)), unwords rest)]
      ("JDouble" : jdouble : rest) -> [(JDouble (literal (read jdouble :: Double)), unwords rest)]
      _ -> error "invalid svalue read"

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
      _ -> error "OrdSymbolic: Cannot compare different SValue types"

{- MEMORY REPRESENTATION -}

-- | Refinement level of pointers
data PtrRefinement = InstPtr | Ptr | ExceptionPtr deriving (Show, Read, Eq)

ptrRefEq :: PtrRefinement -> PtrRefinement -> Bool
ptrRefEq p1 p2 = p1 == p2

-- | Status of the object pointer to, either Null, NotNull or BotPTR (unknown if null or not)
data ObjectStatus = Null | NotNull | BotPTR deriving (Show, Read, Eq)

-- | Models the memory lattice, which is Bot (Whole memory) or a slice (one field/memory address)
data MemLattice = Bot | Slice Integer deriving (Show, Read, Eq)

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
readAddress n = error $ "TODO"

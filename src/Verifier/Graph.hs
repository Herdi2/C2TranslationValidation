-- |
-- - Module: Graph
-- - Exports RawGraph (Used in XML parsing in `GraphParser`) and Graph (SoN graph, used in `Verify` and `GraphBuilder`)
module Verifier.Graph where

import Data.List (findIndex)
import qualified Data.Map as M
import Data.Proxy
import Data.SBV

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
  deriving (Show, Eq)

-- (BlockId, Offset)
type MemIndex = (String, Int64)

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
    ParmMemPtr MemIndex
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
  | -- | Store <Mem id> <address> <value>
    StoreI NodeId NodeId NodeId
  | StoreL NodeId NodeId NodeId
  | StoreF NodeId NodeId NodeId
  | StoreD NodeId NodeId NodeId
  | -- | Load <Mem id> <address>
    LoadI NodeId NodeId
  | LoadL NodeId NodeId
  | LoadF NodeId NodeId
  | LoadD NodeId NodeId
  | -- | MergeMem <Bot memory> [<Alias memory>]
    MergeMem NodeId [NodeId]
  | -- | AddP <ptr1> <ptr2> <offset>, ptr1 := ptr2 + offset
    AddP NodeId NodeId NodeId
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
    methodType :: RetType,
    -- | Maps nodeId (SoN id) to Node
    nodeInfo :: NodeInfo,
    -- | Contains successors for the control flow nodes
    controlSuccessors :: ControlSuccessors,
    -- | Keeps track of which predecessor was last used to enter Region node.
    -- Used to determine value given by Phi nodes.
    regionPredecessor :: M.Map NodeId NodeId,
    -- | Contains unified variables for parameters.
    -- Both to be reused within the graph and unify between the graphs.
    params :: ParamMap
  }
  deriving (Show, Eq)

-- | Graph with default values (everything empty)
-- NOTE: The default return type is `int`
defaultGraph :: Graph
defaultGraph = Graph JINT M.empty M.empty M.empty M.empty

-- | Used to define and keep track of valid method return types
data RetType
  = JINT
  | JLONG
  | JFLOAT
  | JDOUBLE
  deriving (Show, Eq)

mkRetValue :: RetType -> SValue
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

mkGraph :: RetType -> [(NodeId, Node)] -> [(NodeId, [NodeId])] -> Graph
mkGraph retType nInfo successors =
  Graph
    { methodType = retType,
      nodeInfo = M.fromList nInfo,
      controlSuccessors = M.fromList successors,
      regionPredecessor = M.empty,
      params = M.empty
    }

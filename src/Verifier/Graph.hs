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
  | -- | Static calls with own Id data flow predecessor (?)
    CallStatic NodeId NodeId
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
  { -- | Maps nodeId (SoN id) to Node
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

defaultGraph :: Graph
defaultGraph = Graph M.empty M.empty M.empty M.empty

-- | Symbolic values, showing equivalence between Java values and SMT values
data SValue
  = JInt SInt32
  | JLong SInt64
  | -- | FP 8 24
    JFloat SFloat
  | -- | FP 11 53
    JDouble SDouble
  deriving (Show, Eq)

mkGraph :: [(NodeId, Node)] -> [(NodeId, [NodeId])] -> Graph
mkGraph nInfo successors =
  Graph
    { nodeInfo = M.fromList nInfo,
      controlSuccessors = M.fromList successors,
      regionPredecessor = M.empty,
      params = M.empty
    }

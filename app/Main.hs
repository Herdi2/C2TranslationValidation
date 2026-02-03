{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Main (main) where

import Control.Monad
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.SBV
import GHC.Natural

type NodeId = Natural

data BOp = Add | Mul | Sub | LAnd | Cmp deriving (Show, Eq)

data Param = ParamI {getInt :: SInt32} | ParamF {getFloat :: SFloat}

type ParamMap = M.Map NodeId Param

type NodeInfo = M.Map NodeId Node

data Node
  = -- | Parm(Int), carries its own nodeId for parameter handling (see @createParams@)
    ParmI NodeId
  | -- | ConI(int)
    ConI Int32
  | -- | Phi with id and predecessor ids
    Phi NodeId [NodeId]
  | -- | Binary op
    Binop BOp NodeId NodeId
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
    params :: ParamMap
  }

-- | (side effect, return value)
-- e.g. (0, 64) is normal return with value 64
--      (1, 64) is <INSERT EXCEPTION>
type Ret = (SWord32, SInt32)

evalControlNode :: Graph -> Node -> Symbolic Ret
evalControlNode graph@(nodeInfo -> nodes) (Return predId) =
  do
    retVal <- evalDataNode graph (nodes M.! predId)
    return $ (literal 0, retVal)

evalDataNode :: Graph -> Node -> Symbolic SInt32
evalDataNode (params -> parms) (ParmI var) =
  case (M.lookup var parms) of
    Just (ParamI v) -> return v
    Nothing -> error "evalDataNode: Param node wasn't pre-processed"
evalDataNode _ (ConI n) = pure $ literal n
evalDataNode graph@(nodeInfo -> nodes) (Binop bop n1 n2) =
  let val1 = evalDataNode graph (nodes M.! n1)
      val2 = evalDataNode graph (nodes M.! n2)
   in case bop of
        Add -> liftA2 (+) val1 val2
        Sub -> liftA2 (-) val1 val2
        LAnd -> liftA2 (.&.) val1 val2
        Mul -> liftA2 (*) val1 val2
        Cmp ->
          do
            b <- liftA2 (.==) val1 val2
            return $ ite b 1 0
evalDataNode _ n = error $ "Not a data node:" <> show n

mkGraph :: NodeInfo -> Graph
mkGraph nInfo = Graph {nodeInfo = nInfo, params = M.empty}

testGraph :: Graph
testGraph =
  mkGraph $
    M.fromList [(1, ConI 14), (2, ConI 15), (3, Binop Add 1 2), (4, ParmI 4)]

binopGraph :: Graph
binopGraph =
  mkGraph $
    M.fromList
      [ (29, Return 28),
        (28, Binop Add 25 27),
        (25, Binop Mul 10 11),
        (27, Binop Add 10 10),
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
        (24, Binop Sub 23 10),
        (25, Binop Sub 23 11),
        (26, Binop LAnd 24 25),
        (27, Return 26)
      ]

andNegAfter :: Graph
andNegAfter =
  mkGraph $
    M.fromList
      [ (10, ParmI 10),
        (11, ParmI 11),
        (26, Binop LAnd 10 11),
        (27, Return 26)
      ]

-- | Creates a symbolic value for each parameter node.
-- The symbolic values will be reused in the SMT formulas
createParams :: Graph -> Symbolic ParamMap
createParams (nodeInfo -> nodes) = go M.empty (filter isParam $ map snd $ M.toList nodes)
  where
    go paramMap [] = return paramMap
    go paramMap (ParmI nid : rest) =
      do
        param <- ParamI <$> sInt32 ("parm" <> show nid)
        go (M.insert nid param paramMap) rest
    go paramMap (_ : rest) = go paramMap rest

isParam :: Node -> Bool
isParam (ParmI _) = True
isParam _ = False

main :: IO ()
main =
  do
    let before = andNegBefore
    let after = andNegAfter
    res <- sat $
      do
        params <- createParams before
        res1 <- evalControlNode (before {params = params}) (Return 26)
        res2 <- evalControlNode (after {params = params}) (Return 26)
        constrain $ res1 ./= res2
    print res

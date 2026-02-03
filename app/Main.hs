{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Control.Monad
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.SBV
import GHC.Natural

type NodeId = Natural

data BOp = Add | Mul | Sub | LAnd | Cmp deriving (Show, Eq)

data Params = ParamI {getInt :: SInt32} | ParamF {getFloat :: SFloat}

data Node
  = -- | Parm(Int), carries corresponding symbolic value
    ParmI (Maybe SInt32)
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

type Graph = M.Map NodeId Node

-- data Graph
--   = Graph
--   { nodeInfo :: M.Map NodeId Node
--   }

-- | (side effect, return value)
-- e.g. (0, 64) is normal return with value 64
--      (1, 64) is <INSERT EXCEPTION>
type Ret = (SWord32, SInt32)

evalControlNode :: Graph -> Node -> Symbolic Ret
evalControlNode graph (Return predId) =
  do
    retVal <- evalDataNode graph (graph M.! predId)
    return $ (literal 0, retVal)

evalDataNode :: Graph -> Node -> Symbolic SInt32
evalDataNode _ (ParmI var) =
  case var of
    Just v -> return v
    Nothing -> error "evalDataNode: Param node wasn't pre-processed"
evalDataNode _ (ConI n) = pure $ literal n
evalDataNode graph (Binop bop n1 n2) =
  let val1 = evalDataNode graph (graph M.! n1)
      val2 = evalDataNode graph (graph M.! n2)
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

testGraph :: Graph
testGraph =
  M.fromList [(1, ConI 14), (2, ConI 15), (3, Binop Add 1 2), (4, ParmI Nothing)]

binopGraph :: Graph
binopGraph =
  M.fromList
    [ (29, Return 28),
      (28, Binop Add 25 27),
      (25, Binop Mul 10 11),
      (27, Binop Add 10 10),
      (10, ParmI Nothing),
      (11, ParmI Nothing)
    ]

andNegBefore :: Graph
andNegBefore =
  M.fromList
    [ (10, ParmI Nothing),
      (11, ParmI Nothing),
      (23, ConI 0),
      (24, Binop Sub 23 10),
      (25, Binop Sub 23 11),
      (26, Binop LAnd 24 25),
      (27, Return 26)
    ]

andNegAfter :: Graph
andNegAfter =
  M.fromList
    [ (10, ParmI Nothing),
      (11, ParmI Nothing),
      (26, Binop LAnd 10 11),
      (27, Return 26)
    ]

-- | Creates a symbolic value for each parameter node.
-- The symbolic values will be reused in the SMT formulas
createParams :: [(NodeId, Node)] -> Symbolic [(NodeId, Node)]
createParams [] = return []
createParams ((nid, ParmI Nothing) : rest) =
  do
    param <- sInt32 ("param" <> show nid)
    r <- createParams rest
    return $ (nid, ParmI (Just param)) : r
createParams (x : xs) = (x :) <$> createParams xs

-- | Constrains parameters, as they have to be equal
paramEqual :: Graph -> Graph -> Symbolic ()
paramEqual before after =
  do
    let beforeParams = sortOn fst $ filter (isParam . snd) (M.toList before)
    let afterParams = sortOn fst $ filter (isParam . snd) (M.toList after)
    when (length beforeParams /= length afterParams) (error "paramEqual: Length mismatch")
    mapM_ (uncurry constrainParams) (zip beforeParams afterParams)
  where
    constrainParams (parmId1, ParmI (Just parm1)) (parmId2, ParmI (Just parm2)) =
      when (parmId1 /= parmId2) (error "paramEqual: Mismatch param ids")
        >> constrain
        $ parm1 .== parm2

isParam :: Node -> Bool
isParam (ParmI _) = True
isParam _ = False

main :: IO ()
main =
  do
    res <- sat $
      do
        before <- M.fromList <$> createParams (M.toList andNegBefore)
        after <- M.fromList <$> createParams (M.toList andNegAfter)
        res1 <- evalControlNode before (Return 26)
        res2 <- evalControlNode after (Return 26)
        paramEqual before after
        constrain $ res1 ./= res2
    print res

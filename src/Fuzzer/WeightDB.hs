{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Fuzzer.WeightDB where

import qualified Data.Map as M
import Effectful
import Effectful.NonDet
import Effectful.Reader.Static
import Fuzzer.RNG

data GenWeight
  = -- | Field generation weights
    ObjectWeight
  | FieldWeight
  | -- | Statement weights
    DeclareWeight
  | AssignWeight
  | IfElseWeight
  | IfWeight
  | -- | Boolean expression weights
    LogicalConstWeight
  | LogicalExprWeight
  | LogicalCompWeight
  | -- | Expression weights
    VarWeight
  | MemVarWeight
  | ConstExprWeight
  | BinaryExprWeight
  | -- | Arithmetic type weight
    IntWeight
  | LongWeight
  | FloatWeight
  | DoubleWeight
  deriving (Eq, Ord, Show)

type WeightDB = M.Map GenWeight Rational

fieldWeights :: WeightDB
fieldWeights =
  M.fromList [(ObjectWeight, 0.6), (FieldWeight, 0.4)]

stmtWeights :: WeightDB
stmtWeights =
  M.fromList [(DeclareWeight, 0.5), (AssignWeight, 0.3), (IfElseWeight, 0.15), (IfWeight, 0.05)]

logicalWeights :: WeightDB
logicalWeights =
  M.fromList [(LogicalConstWeight, 0.4), (LogicalExprWeight, 0.4), (LogicalCompWeight, 0.2)]

exprWeights :: WeightDB
exprWeights =
  M.fromList [(VarWeight, 0.10), (MemVarWeight, 0.20), (ConstExprWeight, 0.40), (BinaryExprWeight, 0.30)]

arithmeticTypeWeights :: WeightDB
arithmeticTypeWeights =
  M.fromList [(IntWeight, 0.40), (LongWeight, 0.35), (FloatWeight, 0.15), (DoubleWeight, 0.10)]

weights :: WeightDB
weights =
  foldr M.union M.empty [fieldWeights, stmtWeights, logicalWeights, exprWeights, arithmeticTypeWeights]

modifyWeight :: (Reader WeightDB :> es) => (Rational -> Rational) -> GenWeight -> Eff es a -> Eff es a
modifyWeight m g = local $ M.adjust m g

weightedDB :: (NonDet :> es, RNG :> es, Reader WeightDB :> es) => [(Eff es a, GenWeight)] -> Eff es a
weightedDB xs =
  do
    weightdb <- ask
    weightedM $ map (\(gen, weight) -> (gen, weightdb M.! weight)) xs

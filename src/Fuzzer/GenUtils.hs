{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Fuzzer.GenUtils where

import Control.Monad
import Effectful
import Effectful.Labeled
import Effectful.Labeled.State
import Effectful.NonDet

type Fuel = State Integer

type StmtDepth = Labeled "StmtFuel" Fuel

type ExprDepth = Labeled "ExprFuel" Fuel

withExprFuel :: (NonDet :> es, Labeled "ExprFuel" Fuel :> es) => Eff es a -> Eff es a
withExprFuel gen =
  do
    fuel :: Integer <- get @"ExprFuel"
    when (fuel <= (0 :: Integer)) empty
    modify @"ExprFuel" (subtract (1 :: Integer))
    gen

withStmtFuel :: (NonDet :> es, Labeled "StmtFuel" Fuel :> es) => Eff es a -> Eff es a
withStmtFuel gen =
  do
    fuel :: Integer <- get @"StmtFuel"
    when (fuel <= (0 :: Integer)) empty
    modify @"StmtFuel" (subtract (1 :: Integer))
    gen

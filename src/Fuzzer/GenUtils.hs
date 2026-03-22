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
import Effectful.Labeled.Reader
import Effectful.NonDet

type Fuel = Reader Integer

type StmtDepth = Labeled "StmtFuel" Fuel

type ExprDepth = Labeled "ExprFuel" Fuel

withExprFuel :: (NonDet :> es, ExprDepth :> es) => Eff es a -> Eff es a
withExprFuel gen =
  do
    fuel :: Integer <- ask @"ExprFuel"
    when (fuel <= (0 :: Integer)) empty
    local @"ExprFuel" (subtract (1 :: Integer)) gen

putExprFuel :: (ExprDepth :> es) => Integer -> Eff es a -> Eff es a
putExprFuel fuel gen = local @"ExprFuel" (const fuel) $ gen

putStmtFuel :: (StmtDepth :> es) => Integer -> Eff es a -> Eff es a
putStmtFuel fuel gen = local @"StmtFuel" (const fuel) $ gen

withStmtFuel :: (NonDet :> es, StmtDepth :> es) => Eff es a -> Eff es a
withStmtFuel gen =
  do
    fuel :: Integer <- ask @"StmtFuel"
    when (fuel <= (0 :: Integer)) empty
    local @"StmtFuel" (subtract (1 :: Integer)) gen

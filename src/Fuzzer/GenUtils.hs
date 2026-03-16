{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Fuzzer.GenUtils where

import Control.Monad
import Effectful
import Effectful.Labeled
import Effectful.Labeled.State
import Effectful.NonDet

type StmtDepth = Labeled "StmtFuel" Fuel

type Fuel = State Integer

type ExprDepth = Labeled "ExprFuel" Fuel

withFuel :: forall label a es. (NonDet :> es, Labeled label Fuel :> es) => Eff es a -> Eff es a
withFuel gen =
    do
    fuel :: Integer <- get @label
    when (fuel <= (0 :: Integer)) empty
    gen


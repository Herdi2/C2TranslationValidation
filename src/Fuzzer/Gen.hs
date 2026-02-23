{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Fuzzer.Gen where

import Data.Word
import Effectful
import Fuzzer.Grammar
import Fuzzer.RNG

type GenEffects =
  '[ RNG
   ]

type Gen a = Eff GenEffects a

runGen :: Word64 -> Gen a -> a
runGen seed = runPureEff . runRNG seed

expr :: Gen JExpr
expr =
  weighted
    [ (con, 0.5)
    ]

var :: Gen JExpr
var = return $ JVariable JInt "a"

con :: Gen JExpr
con = return $ JConst JInt (IntLit 10)

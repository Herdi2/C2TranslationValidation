{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Data.List
import Data.Maybe
import Data.SBV

type Id = Integer

data Node
  = Region Id [Integer] Node Node
  | ConI Integer
  | CmpI Node Node
  | Phi Id [Node]
  | If Id Node Node
  | IfT Id Node
  | IfF Id Node
  | Return Id Node

mkSymbolic [''Node]

type Env = [(Id, Int)]

envFind :: Id -> Env -> Int
envFind _ [] = error "envFind"
envFind i ((x, val) : xs) =
  if i == x then val else envFind i xs

getId :: Node -> Id
getId (Region x _ _ _) = x
getId (Phi x _) = x
getId (If x _ _) = x
getId (IfT x _) = x
getId (IfF x _) = x
getId (Return x _) = x
getId _ = error "getId"

eval :: Env -> Node -> Integer
eval env (ConI n) = n
eval env (CmpI n1 n2) = if eval env n1 /= eval env n2 then 1 else 0
eval env (Phi pId inputs) =
  eval env (inputs !! (envFind pId env))
eval env (Region rId inputs pred succ) =
  let inputIndex = findIndex ((==) (getId pred)) inputs
   in eval ((rId, fromJust inputIndex) : env) succ

ret :: SWord64 -> SWord64
ret = uninterpret "ret"

main :: IO ()
main =
  do
    res <- sat $
      do
        x <- free "x"
        constrain $ x .< (0 :: SWord8)
        constrain $ x .> (0 :: SWord8)

    print res

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Fuzzer.RNG where

import Data.Word
import Effectful
import Effectful.Dispatch.Static
import System.Random

data RNG :: Effect

type instance DispatchOf RNG = Static NoSideEffects

newtype instance StaticRep RNG = RNG StdGen

runRNG :: Word64 -> Eff (RNG : es) a -> Eff es a
runRNG seed = evalStaticRep $ RNG (mkStdGen64 seed)

-- | Returns a random value within within the valid range of the type
rand :: (Random a, RNG :> es) => Eff es a
rand =
  do
    RNG g <- getStaticRep
    let (res, newGen) = random g
    putStaticRep (RNG newGen)
    return res

-- | Returns a random value within within the given range
randR :: (Random a, RNG :> es) => (a, a) -> Eff es a
randR (a, b) =
  do
    RNG g <- getStaticRep
    let (res, newGen) = randomR (a, b) g
    putStaticRep (RNG newGen)
    return res

-- | Sample a random value from a weighted list.  The total weight of all
-- elements must not be 0.
-- Taken from: https://hackage.haskell.org/package/MonadRandom-0.1.3/docs/src/Control-Monad-Random.html#fromList
weighted :: (RNG :> es) => [(Eff es a, Rational)] -> Eff es a
weighted [] = error "Internal error: fromList called with empty list"
weighted [(x, _)] = x >>= return
weighted xs = do
  -- TODO: Better error message if weights sum to 0.
  let s = (fromRational (sum (map snd xs))) :: Double -- total weight
      cs = scanl1 (\(_, q) (y, s') -> (y, s' + q)) xs -- cumulative weight
  p <- toRational <$> randR (0.0, s)
  genVal <- fst . head $ dropWhile (\(_, q) -> q < p) cs
  return genVal

choose :: (RNG :> es) => [Eff es a] -> Eff es a
choose = weighted . fmap (,1)

{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : Main
-- Copyright   : (c) The President and Fellows of Harvard College 2009-2010
-- Copyright   : (c) Geoffrey Mainland 2012
-- License     : BSD-style
--
-- Maintainer  : Geoffrey Mainland <mainland@apeiron.net>
-- Stability   : experimental
-- Portability : non-portable

module Main where

import Prelude hiding (map, zipWith, zipWith3)

import Control.DeepSeq
import qualified Control.Exception as E
import qualified Criterion as C
import qualified Criterion.Main as C
import qualified Data.Vector.Storable as V
import Foreign (Storable)
import Text.Printf

import qualified Data.Array.Nikola.Backend.CUDA.Haskell as NH
import qualified Data.Array.Nikola.Backend.CUDA.TH as NTH
import Data.Array.Nikola.Util.Random

import qualified BlackScholes.Nikola as BSN
import qualified BlackScholes.Vector as BSV

type F = Float

rISKFREE :: F
rISKFREE = 0.02

vOLATILITY :: F
vOLATILITY = 0.30;

main :: IO ()
main = do
  benchmarks <- mapM benchmarksForN [0,2..20]
  C.defaultMain benchmarks

benchmarksForN :: Double -> IO C.Benchmark
benchmarksForN logn = do
    (ss, xs, ts) <- generateData n
    return $ C.bgroup (printf "2**%-2.0f" logn)
               [ C.bench (printf "            vector 2**%-2.0f" logn) $
                       C.nf blackscholesVector (ss, xs, ts)
               , C.bench (printf "nikola interpreter 2**%-2.0f" logn) $
                       C.nf blackscholesNikola (ss, xs, ts)
               , C.bench (printf "   nikola compiled 2**%-2.0f" logn) $
                       C.nf blackscholesNikolaCompiled (ss, xs, ts)
               ]
  where
    n :: Int
    n = truncate (2**logn)

generateData :: Int -> IO (V.Vector F, V.Vector F, V.Vector F)
generateData n = do
    ss <- randomsRange n (5.0, 30.0)
    xs <- randomsRange n (1.0, 100.0)
    ts <- randomsRange n (0.25, 10.0)
    E.evaluate ss
    E.evaluate xs
    E.evaluate ts
    return (ss, xs, ts)

blackscholesNikola :: (V.Vector F, V.Vector F, V.Vector F)
                   -> V.Vector F
blackscholesNikola (ss, xs, ts) =
    NH.compile BSN.blackscholes ss xs ts rISKFREE vOLATILITY

blackscholesNikolaCompiled :: (V.Vector F, V.Vector F, V.Vector F)
                           -> V.Vector F
blackscholesNikolaCompiled (ss, xs, ts) =
    blackscholes ss xs ts rISKFREE vOLATILITY
  where
    blackscholes :: V.Vector F
                 -> V.Vector F
                 -> V.Vector F
                 -> F
                 -> F
                 -> V.Vector F
    blackscholes = $(NTH.compileSig BSN.blackscholes (undefined :: V.Vector F
                                                                -> V.Vector F
                                                                -> V.Vector F
                                                                -> F
                                                                -> F
                                                                -> V.Vector F))

blackscholesVector :: (V.Vector F, V.Vector F, V.Vector F)
                   -> V.Vector F
{-# INLINE blackscholesVector #-}
blackscholesVector (ss, xs, ts) =
    V.zipWith3 (\s x t -> BSV.blackscholes True s x t rISKFREE vOLATILITY) ss xs ts

instance Storable a => NFData (V.Vector a) where
    rnf v = V.length v `seq` ()

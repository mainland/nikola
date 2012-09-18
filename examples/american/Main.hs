{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : Main
-- Copyright   : (c) Geoffrey Mainland 2012
-- License     : BSD-style
--
-- Maintainer  : Geoffrey Mainland <mainland@apeiron.net>
-- Stability   : experimental
-- Portability : non-portable

module Main where

import Prelude hiding (map, zipWith, zipWith3)
import qualified Prelude as P

import Control.Monad
import qualified Criterion as C
import qualified Criterion.Main as C
import Data.Int
import System.Environment
import Text.Printf

import Data.List (foldl')

import qualified Data.Array.Nikola.Backend.CUDA.Haskell as NH
import qualified Data.Array.Nikola.Backend.CUDA.TH as NTH
import Data.Array.Nikola.Util.Statistics

import qualified Data.Vector.CUDA.Storable as CV
import qualified Data.Vector.Storable as V

import qualified American.Nikola as AMN
import qualified American.Vector as AMV

type F = Float

main :: IO ()
main = do
  args <- System.Environment.getArgs
  case args of
    ["--validate"] -> validate
    _              -> C.defaultMain $
                      P.map benchmarksForN [1, 8, 16, 30, 64, 128, 256]

benchmarksForN :: Int -> C.Benchmark
benchmarksForN n =
    C.bgroup (show n)
         [ C.bench (printf "            vector %-3d" n) $
                   C.nf AMV.binom n
         , C.bench (printf "nikola interpreter %-3d" n) $
                   C.nf nikolaBinom n
         , C.bench (printf "   nikola compiled %-3d" n) $
                   C.nf nikolaBinomCompiled n
         ]

nikolaBinom :: Int -> F
nikolaBinom expiry = V.head (CV.toHostVector first)
  where
    uPow :: CV.Vector F
    uPow = CV.fromHostVector $ V.generate (n+1) (u^)

    dPow :: CV.Vector F
    dPow = CV.fromHostVector $ V.reverse $ V.generate (n+1) (d^)

    first :: CV.Vector F
    first = foldl' (prevPut uPow dPow (fromIntegral expiry)) (finalPut uPow dPow)
              [fromIntegral n, fromIntegral n-1 .. 1]

    finalPut :: CV.Vector F
             -> CV.Vector F
             -> CV.Vector F
    finalPut = NH.compile AMN.finalPut

    prevPut :: CV.Vector F
            -> CV.Vector F
            -> Int32
            -> CV.Vector F
            -> Int32
            -> CV.Vector F
    prevPut = NH.compile AMN.prevPut

    -- standard econ parameters
    bankDays = 252
    alpha = 0.07; sigma = 0.20

    n = expiry*bankDays
    dt = fromIntegral expiry/fromIntegral n
    u = exp(alpha*dt+sigma*sqrt dt)
    d = exp(alpha*dt-sigma*sqrt dt)

nikolaBinomCompiled :: Int -> F
nikolaBinomCompiled expiry = V.head (CV.toHostVector first)
  where
    uPow :: CV.Vector F
    uPow = CV.fromHostVector $ V.generate (n+1) (u^)

    dPow :: CV.Vector F
    dPow = CV.fromHostVector $ V.reverse $ V.generate (n+1) (d^)

    first :: CV.Vector F
    first = foldl' (prevPut uPow dPow (fromIntegral expiry)) (finalPut uPow dPow)
              [fromIntegral n, fromIntegral n-1 .. 1]

    finalPut :: CV.Vector F
             -> CV.Vector F
             -> CV.Vector F
    finalPut = $(NTH.compileSig AMN.finalPut (undefined :: CV.Vector F
                                                        -> CV.Vector F
                                                        -> CV.Vector F))

    prevPut :: CV.Vector F
            -> CV.Vector F
            -> Int32
            -> CV.Vector F
            -> Int32
            -> CV.Vector F
    prevPut = $(NTH.compileSig AMN.prevPut (undefined :: CV.Vector F
                                                      -> CV.Vector F
                                                      -> Int32
                                                      -> CV.Vector F
                                                      -> Int32
                                                      -> CV.Vector F))

    -- standard econ parameters
    bankDays = 252
    alpha = 0.07; sigma = 0.20

    n = expiry*bankDays
    dt = fromIntegral expiry/fromIntegral n
    u = exp(alpha*dt+sigma*sqrt dt)
    d = exp(alpha*dt-sigma*sqrt dt)

validate :: IO ()
validate =
    forM_ [1, 8, 16, 30, 64, 128, 256] $ \n -> do
      let v1 = AMV.binom n
      let v2 = nikolaBinomCompiled n
      validateL1Norm ePSILON (printf "expiry %-3d" n) (V.singleton v1) (V.singleton v2)
  where
    ePSILON :: F
    ePSILON = 1e-10

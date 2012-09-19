{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module American.Nikola (
    binom,
    binomCompiled
  ) where

import Data.Int
import Data.List (foldl')

import qualified Data.Vector.CUDA.Storable as CV
import qualified Data.Vector.Storable as V

import qualified Data.Array.Nikola.Backend.CUDA.Haskell as NH
import qualified Data.Array.Nikola.Backend.CUDA.TH as NTH

import qualified American.Nikola.Implementation as Imp

--
-- This code is adapted from Ken Friis Larsen's implementation of pricing for
-- American options available at <http://github.com/kfl/american-options>.
--

type F = Double

binom :: Int -> F
binom expiry = V.head (CV.toHostVector first)
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
    finalPut = NH.compile Imp.finalPut

    prevPut :: CV.Vector F
            -> CV.Vector F
            -> Int32
            -> CV.Vector F
            -> Int32
            -> CV.Vector F
    prevPut = NH.compile Imp.prevPut

    -- standard econ parameters
    bankDays = 252
    alpha = 0.07; sigma = 0.20

    n = expiry*bankDays
    dt = fromIntegral expiry/fromIntegral n
    u = exp(alpha*dt+sigma*sqrt dt)
    d = exp(alpha*dt-sigma*sqrt dt)

binomCompiled :: Int -> F
binomCompiled expiry = V.head (CV.toHostVector first)
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
    finalPut = $(NTH.compileSig Imp.finalPut (undefined :: CV.Vector F
                                                        -> CV.Vector F
                                                        -> CV.Vector F))

    prevPut :: CV.Vector F
            -> CV.Vector F
            -> Int32
            -> CV.Vector F
            -> Int32
            -> CV.Vector F
    prevPut = $(NTH.compileSig Imp.prevPut (undefined :: CV.Vector F
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

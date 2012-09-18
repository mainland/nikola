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

import Control.Monad
import Control.Monad.Trans (liftIO)
import qualified Criterion as C
import qualified Criterion.Config as C
import qualified Criterion.Environment as C
import qualified Criterion.Main as C
import qualified Criterion.Monad as C
import Data.Int
import qualified Statistics.Function as S
import qualified Statistics.Sample as S
import System.Environment
import Text.Printf

import Data.List (foldl')

import qualified Data.Array.Nikola.Backend.CUDA.Haskell as NH
import qualified Data.Array.Nikola.Backend.CUDA.TH as NTH

import qualified Data.Vector.CUDA.Storable as CV
import qualified Data.Vector.Storable as V

import qualified American.Nikola as AMN
import qualified American.Vector as AMV

type F = Float

main :: IO ()
main = do
    (cfg, _) <- System.Environment.getArgs >>=
                C.parseArgs C.defaultConfig C.defaultOptions
    C.withConfig cfg $ do
        env <- C.measureEnvironment
        forM_ [1, 8, 16, 30, 64, 128, 256] $ \n -> do
            samplesVector         <- C.runBenchmark env (C.nf AMV.binom n)
            samplesNikola         <- C.runBenchmark env (C.nf nikolaBinom n)
            samplesNikolaCompiled <- C.runBenchmark env (C.nf nikolaBinomCompiled n)
            liftIO $ printf "%d" n
            liftIO $ printSample samplesVector
            liftIO $ printSample samplesNikola
            liftIO $ printSample samplesNikolaCompiled
            liftIO $ printf "\n"
  where
    printSample :: S.Sample -> IO ()
    printSample samp =
        printf ",%0.2e,%0.2e,%0.2e" mu min max
      where
        mu         = S.mean samp
        (min, max) = S.minMax samp

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

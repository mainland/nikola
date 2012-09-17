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

import Control.Monad
import Control.Monad.Trans (liftIO)
import Criterion
import Criterion.Config
import Criterion.Environment
import Criterion.Main
import Criterion.Monad
import qualified Data.Vector.Storable as V
import Statistics.Function
import Statistics.Sample
import System.Environment
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
    (cfg, _) <- parseArgs defaultConfig defaultOptions =<< System.Environment.getArgs
    withConfig cfg $ do
        env <- measureEnvironment
        -- Powers of two up to 32MB
        forM_ [0..25] $ \e -> do
            let n = truncate (2**e)
            samplesNikola         <- runBenchmark env
                                     (benchmarkBlackScholes blackscholesNikola n)
            samplesNikolaCompiled <- runBenchmark env
                                     (benchmarkBlackScholes blackscholesNikolaCompiled n)
            samplesVector         <- runBenchmark env
                                     (benchmarkBlackScholes blackscholesVector n)
            liftIO $ printf "%d" n
            liftIO $ printSample samplesNikola
            liftIO $ printSample samplesNikolaCompiled
            liftIO $ printSample samplesVector
            liftIO $ printf "\n"
  where
    printSample :: Sample -> IO ()
    printSample samp =
        printf ",%0.2e,%0.2e,%0.2e" mu min max
      where
        mu         = mean samp
        (min, max) = minMax samp

benchmarkBlackScholes :: (   V.Vector F
                          -> V.Vector F
                          -> V.Vector F
                          -> V.Vector F)
                      -> Int
                      -> IO (V.Vector F)
benchmarkBlackScholes f n =
    benchmarkBlackScholesIO (\ss xs ts -> return $! f xs ss ts) n

benchmarkBlackScholesIO :: (   V.Vector F
                            -> V.Vector F
                            -> V.Vector F
                            -> IO (V.Vector F))
                         -> Int
                         -> IO (V.Vector F)
benchmarkBlackScholesIO f n = do
    ss <- randomsRange n (5.0, 30.0)
    xs <- randomsRange n (1.0, 100.0)
    ts <- randomsRange n (0.25, 10.0)
    f ss xs ts

blackscholesNikola :: V.Vector F
                   -> V.Vector F
                   -> V.Vector F
                   -> V.Vector F
blackscholesNikola ss xs ts =
    NH.compile BSN.blackscholes ss xs ts rISKFREE vOLATILITY

blackscholesNikolaCompiled :: V.Vector F
                           -> V.Vector F
                           -> V.Vector F
                           -> V.Vector F
blackscholesNikolaCompiled ss xs ts =
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

blackscholesVector :: V.Vector F
                   -> V.Vector F
                   -> V.Vector F
                   -> V.Vector F
blackscholesVector ss xs ts =
    V.zipWith3 (\s x t -> BSV.blackscholes True s x t rISKFREE vOLATILITY) ss xs ts

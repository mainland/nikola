-- Copyright (c) 2010
--         The President and Fellows of Harvard College.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions
-- are met:
-- 1. Redistributions of source code must retain the above copyright
--    notice, this list of conditions and the following disclaimer.
-- 2. Redistributions in binary form must reproduce the above copyright
--    notice, this list of conditions and the following disclaimer in the
--    documentation and/or other materials provided with the distribution.
-- 3. Neither the name of the University nor the names of its contributors
--    may be used to endorse or promote products derived from this software
--    without specific prior written permission.

-- THIS SOFTWARE IS PROVIDED BY THE UNIVERSITY AND CONTRIBUTORS ``AS IS'' AND
-- ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
-- IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
-- ARE DISCLAIMED.  IN NO EVENT SHALL THE UNIVERSITY OR CONTRIBUTORS BE LIABLE
-- FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
-- OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
-- HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
-- LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
-- OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
-- SUCH DAMAGE.

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Prelude hiding (map, zipWith, zipWith3)
import qualified Prelude as P

import CUDA.Context
import Control.Monad
import Control.Monad.Trans (liftIO)
import Criterion
import Criterion.Config
import Criterion.Environment
import Criterion.Main
import Criterion.Monad
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Storable as V
import Data.Vector.Random.Mersenne
import qualified Data.Vector.Fusion.Stream         as Stream
import qualified Data.Vector.Fusion.Stream.Monadic as S
import qualified Data.Vector.Fusion.Stream.Size    as S
import Data.Vector.Fusion.Util
import Statistics.Function
import Statistics.Sample
import System.Environment
import System.Random.Mersenne.Pure64
-- import Text.PrettyPrint.Mainland
import Text.Printf

import Nikola
import qualified Nikola.Syntax
import qualified Nikola.ToC

import qualified BlackScholes.CUDA as BSC
import qualified BlackScholes.Nikola as BSN
import qualified BlackScholes.Nikola2 as BSN2
import qualified BlackScholes.Vector as BSV

rISKFREE :: Float
rISKFREE = 0.02

vOLATILITY :: Float
vOLATILITY = 0.30;

main :: IO ()
main = withNewContext $ \_ -> do
    (cfg, _) <- parseArgs defaultConfig defaultOptions =<< System.Environment.getArgs
    withConfig cfg $ do
        env <- measureEnvironment
        -- Powers of two up to 256MB
        forM_ [0..25] $ \e -> do
            let n = truncate (2**e)
            samplesCUDA           <- runBenchmark env
                                     (benchmarkBlackScholes blackscholesCUDA n)
            samplesNikola         <- runBenchmark env
                                     (benchmarkBlackScholesIO blackscholesNikola n)
            samplesNikolaCompiledNoObservedSharingNoVapply <-
                                     runBenchmark env
                                     (benchmarkBlackScholes blackscholesNikolaCompiledNoObservedSharingNoVapply n)
            samplesNikolaCompiledNoVapply <-
                                     runBenchmark env
                                     (benchmarkBlackScholes blackscholesNikolaCompiledNoVapply n)
            samplesNikolaCompiled <- runBenchmark env
                                     (benchmarkBlackScholes blackscholesNikolaCompiled n)
            samplesVector         <- runBenchmark env
                                     (benchmarkBlackScholes blackscholesVector n)
            liftIO $ printf "%d" n
            liftIO $ printSample samplesCUDA
            liftIO $ printSample samplesNikola
            liftIO $ printSample samplesNikolaCompiledNoObservedSharingNoVapply
            liftIO $ printSample samplesNikolaCompiledNoVapply
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

benchmarkBlackScholes :: (   V.Vector Float
                          -> V.Vector Float
                          -> V.Vector Float
                          -> V.Vector Float)
                      -> Int
                      -> IO (V.Vector Float)
benchmarkBlackScholes f n =
    benchmarkBlackScholesIO (\ss xs ts -> return $! f xs ss ts) n

benchmarkBlackScholesIO :: (   V.Vector Float
                            -> V.Vector Float
                            -> V.Vector Float
                            -> IO (V.Vector Float))
                         -> Int
                         -> IO (V.Vector Float)
benchmarkBlackScholesIO f n = do
    g <- liftIO newPureMT
    let ss = randomsRange g 5.0 30.0 n :: V.Vector Float
    g <- liftIO newPureMT
    let xs = randomsRange g 1.0 100.0 n :: V.Vector Float
    g <- liftIO newPureMT
    let ts = randomsRange g 0.25 10.0 n :: V.Vector Float
    f ss xs ts

blackscholesCUDA :: V.Vector Float
                 -> V.Vector Float
                 -> V.Vector Float
                 -> V.Vector Float
blackscholesCUDA ss xs ts =
    blackscholes ss xs ts rISKFREE vOLATILITY
  where
    blackscholes :: V.Vector Float
                 -> V.Vector Float
                 -> V.Vector Float
                 -> Float
                 -> Float
                 -> V.Vector Float
    blackscholes = $(compileTH BSC.blackscholes)

blackscholesNikola :: V.Vector Float
                   -> V.Vector Float
                   -> V.Vector Float
                   -> IO (V.Vector Float)
blackscholesNikola ss xs ts =
    call BSN.blackscholes ss xs ts rISKFREE vOLATILITY

blackscholesNikolaCompiledNoObservedSharingNoVapply :: V.Vector Float
                                                    -> V.Vector Float
                                                    -> V.Vector Float
                                                    -> V.Vector Float
blackscholesNikolaCompiledNoObservedSharingNoVapply ss xs ts =
    blackscholes ss xs ts rISKFREE vOLATILITY
  where
    blackscholes :: V.Vector Float
                 -> V.Vector Float
                 -> V.Vector Float
                 -> Float
                 -> Float
                 -> V.Vector Float
    blackscholes = $(compileTH' (ROpts False) BSN2.blackscholes)

blackscholesNikolaCompiledNoVapply :: V.Vector Float
                                   -> V.Vector Float
                                   -> V.Vector Float
                                   -> V.Vector Float
blackscholesNikolaCompiledNoVapply ss xs ts =
    blackscholes ss xs ts rISKFREE vOLATILITY
  where
    blackscholes :: V.Vector Float
                 -> V.Vector Float
                 -> V.Vector Float
                 -> Float
                 -> Float
                 -> V.Vector Float
    blackscholes = $(compileTH' (ROpts True) BSN2.blackscholes)

blackscholesNikolaCompiled :: V.Vector Float
                           -> V.Vector Float
                           -> V.Vector Float
                           -> V.Vector Float
blackscholesNikolaCompiled ss xs ts =
    blackscholes ss xs ts rISKFREE vOLATILITY
  where
    blackscholes :: V.Vector Float
                 -> V.Vector Float
                 -> V.Vector Float
                 -> Float
                 -> Float
                 -> V.Vector Float
    blackscholes = $(compileTH BSN.blackscholes)

blackscholesVector :: V.Vector Float
                   -> V.Vector Float
                   -> V.Vector Float
                   -> V.Vector Float
blackscholesVector ss xs ts =
    V.zipWith3 (\s x t -> BSV.blackscholes True s x t rISKFREE vOLATILITY) ss xs ts

randomsRange :: forall a v . (RealFloat a, PureMTRandom a, G.Vector v a)
             => PureMT
             -> a
             -> a
             -> Int
             -> v a
randomsRange g hi lo n =
    G.unstream (randomS g n)
  where
    scale :: Int -> a
    scale x = (1.0 - t) * lo + t * hi
      where
        t :: a
        t = fromIntegral x / fromIntegral (maxBound :: Int)

    randomS :: PureMT -> Int -> Stream.Stream a
    {-# INLINE [1] randomS #-}
    randomS g n = S.Stream (return . step) (n, g) (S.Exact (delay_inline max n 0))
      where
        {-# INLINE [0] step #-}
        step (i,g) | i <= 0    = S.Done
                   | otherwise = g `seq` case random g of
                                    (r, g') -> S.Yield (scale r) (i-1, g')

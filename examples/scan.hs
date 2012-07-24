-- Copyright (c) 2010-2012
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

import Control.Monad
import Control.Monad.Trans (liftIO)
import Criterion
import Criterion.Config
import Criterion.Environment
import Criterion.Main
import Criterion.Monad
import Data.Int
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Storable as V
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

import Data.Array.Nikola
import Data.Array.Nikola.Util

main :: IO ()
main = withNewContext $ \_ -> do
    (cfg, _) <- parseArgs defaultConfig defaultOptions =<< System.Environment.getArgs
    withConfig cfg $ do
        env <- measureEnvironment
        -- Powers of two up to 256MB
        forM_ [0..28] $ \e -> do
            let n = truncate (2**e)
            samplesNikola         <- runBenchmark env
                                     (benchmarkIO scanNikola n)
            samplesVector         <- runBenchmark env
                                     (benchmarkIO scanVector n)
            liftIO $ printf "%d" n
            liftIO $ printSample samplesNikola
            liftIO $ printSample samplesVector
            liftIO $ printf "\n"
  where
    printSample :: Sample -> IO ()
    printSample samp =
        printf ",%0.2e,%0.2e,%0.2e" mu min max
      where
        mu         = mean samp
        (min, max) = minMax samp

benchmarkIO :: (V.Vector Int32 -> IO (V.Vector Int32))
            -> Int
            -> IO (V.Vector Int32)
benchmarkIO f n = do
    g <- liftIO newPureMT
    let flags = randomFlags g n
    f flags

scanNikola :: V.Vector Int32
           -> IO (V.Vector Int32)
scanNikola flags = do
    v <- toRep flags
    scan' v
    fromRep v
  where
    scan' :: Vector Int32 -> IO ()
    scan'  (Vector 0 _)  = return ()
    scan'  xs            = do  sums <- bscan xs
                               scan' sums
                               badd xs sums

    bscan :: Vector Int32 -> IO (Vector Int32)
    bscan = compile (blockedScanM (+) 0)

    badd :: Vector Int32 -> Vector Int32 -> IO ()
    badd = compile blockedAddM

scanVector :: V.Vector Int32
           -> IO (V.Vector Int32)
scanVector flags =
    return $! V.prescanl' (+) 0 flags

randomFlags :: forall v . (G.Vector v Int32)
            => PureMT
            -> Int
            -> v Int32
randomFlags g n =
    G.unstream (randomS g n)
  where
    scale :: Int -> Int32
    scale x = fromIntegral (x `mod` 2)

    randomS :: PureMT -> Int -> Stream.Stream Int32
    {-# INLINE [1] randomS #-}
    randomS g n = S.Stream (return . step) (n, g) (S.Exact (delay_inline max n 0))
      where
        {-# INLINE [0] step #-}
        step (i,g) | i <= 0    = S.Done
                   | otherwise = g `seq` case random g of
                                    (r, g') -> S.Yield (scale r) (i-1, g')

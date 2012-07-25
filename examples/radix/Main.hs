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

import Prelude hiding (map, mapM, zipWith, zipWith3)

import Control.Monad (forM_)
import Control.Monad.Trans (liftIO)
import Criterion
import Criterion.Config
import Criterion.Environment
import Criterion.Main
import Criterion.Monad
import Data.Bits (shiftL)
import Data.Int
import qualified Data.Vector.Algorithms.Radix as R
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as M
import Foreign.CUDA.Driver.Marshal
import Foreign.Storable
import Statistics.Function
import Statistics.Sample
import System.Environment
-- import Text.PrettyPrint.Mainland
import Text.Printf

import Data.Array.Nikola.Backend.CUDA
import Data.Array.Nikola.Util

import Data.Array.Repa hiding ((++))
import Data.Array.Repa.Repr.CUDA

main :: IO ()
main = withNewContext $ \_ -> do
    (cfg, _) <- parseArgs defaultConfig defaultOptions =<<
                System.Environment.getArgs
    withConfig cfg $ do
        env <- measureEnvironment
        -- Powers of two up to 256MB
        forM_ [0..28] $ \e -> do
            let n = truncate (2**e)
            samplesNikola         <- runBenchmark env
                                     (benchmarkIO radixNikola n)
            samplesVector         <- runBenchmark env
                                     (benchmarkIO radixVector n)
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

benchmarkIO :: (V.Vector Int32 -> IO a)
            -> Int
            -> IO a
benchmarkIO f n = do
    v <- randoms n
    f v

radixNikola :: V.Vector Int32
            -> IO (V.Vector Int32)
radixNikola xs = do
    v <- toCUDARep xs
    radixM v
    fromCUDARep v

withNewCUDAArray :: Storable a => Int -> (Array CU DIM1 a -> IO b) -> IO b
withNewCUDAArray n kont  =
    allocaArray n $ \devptr -> do
    kont (fromDevicePtr (Z:.n) devptr)

radixM :: Array CU DIM1 Int32
       -> IO ()
radixM xs@(ADevicePtr _ n _) =
    withNewCUDAArray n $ \bits -> do
    forM_ [0..31] $ \bit -> do
        bitM (1 `shiftL` bit) xs bits
        splitM xs bits

splitM :: Array CU DIM1 Int32
       -> Array CU DIM1 Int32
       -> IO ()
splitM a@(ADevicePtr _ n _) flags =
    withNewCUDAArray n $ \(nflags :: Array CU DIM1 Int32) ->
    withNewCUDAArray n $ \(up :: Array CU DIM1 Int32) ->
    withNewCUDAArray n $ \(index :: Array CU DIM1 Int32) -> do
    let down = nflags
    copyM flags up
    notM flags nflags
    -- dbg "flags" flags
    -- dbg "nflags" nflags
    plusScanM nflags
    -- dbg "down" down
    plusNacsM up
    -- dbg "up" up
    flipM (fromIntegral n) up up
    -- dbg "up" up
    chooseM flags up down index
    -- dbg "index" index
    permuteIntM a index a

bitM :: Int32
     -> Array CU DIM1 Int32
     -> Array CU DIM1 Int32
     -> IO ()
bitM = compile (\b  -> mapM (\x -> ((x .&. b) ./=. 0) ? (1, 0)))

permuteIntM :: Array CU DIM1 Int32
            -> Array CU DIM1 Int32
            -> Array CU DIM1 Int32
            -> IO ()
permuteIntM = compile permuteM

plusScanM :: Array CU DIM1 Int32
          -> IO ()
plusScanM xs = do
    scan' xs
  where
    scan' :: Array CU DIM1 Int32 -> IO ()
    scan'  xs | extent xs == (Z:.0) = return ()
    scan'  xs                       = do  sums <- plusBlockedScanM xs
                                          scan' sums
                                          blockedSumM xs sums

plusNacsM :: Array CU DIM1 Int32
          -> IO ()
plusNacsM xs = do
    nacs' xs
  where
    nacs' :: Array CU DIM1 Int32 -> IO ()
    nacs'  xs | extent xs == (Z:.0) = return ()
    nacs'  xs                       = do  sums <- plusBlockedNacsM xs
                                          nacs' sums
                                          blockedSumM xs sums

plusBlockedScanM :: Array CU DIM1 Int32 -> IO (Array CU DIM1 Int32)
plusBlockedScanM = compile (blockedScanM (+) 0)

plusBlockedNacsM :: Array CU DIM1 Int32 -> IO (Array CU DIM1 Int32)
plusBlockedNacsM = compile (blockedNacsM (+) 0)

blockedSumM :: Array CU DIM1 Int32 -> Array CU DIM1 Int32 -> IO ()
blockedSumM = compile blockedAddM

copyM :: Array CU DIM1 Int32 -> Array CU DIM1 Int32 -> IO ()
copyM = compile (mapM id)

notM :: Array CU DIM1 Int32 -> Array CU DIM1 Int32 -> IO ()
notM = compile (mapM (\x -> (x .==. 0) ? (1, 0)))

flipM :: Int32 -> Array CU DIM1 Int32 -> Array CU DIM1 Int32 -> IO ()
flipM = compile (\n -> mapM (\x -> n - 1 - x))

chooseM :: Array CU DIM1 Int32
        -> Array CU DIM1 Int32
        -> Array CU DIM1 Int32
        -> Array CU DIM1 Int32
        -> IO ()
chooseM = compile (zipWith3M (\ x y z -> (x ./=. 0) ? (y, z)))

dbg :: String -> Array CU DIM1 Int32 -> IO ()
dbg msg xs = do
    xs' :: [Int32] <- fromCUDARep xs
    putStrLn $ msg ++ ": " ++ show xs'

radixVector :: V.Vector Int32
            -> IO ()
radixVector xs = do
    xs' <- M.new (V.length xs)
    forM_ [0..V.length xs-1] $ \i -> M.write xs' i (xs V.! i)
    R.sort xs'


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

import Prelude hiding (map, mapM, zipWith, zipWith3)
import qualified Prelude as P

import CUDA.Context
import Control.Monad (forM_)
import Control.Monad.Trans (liftIO)
import Criterion
import Criterion.Config
import Criterion.Environment
import Criterion.Main
import Criterion.Monad
import Data.Array.Vector
import Data.Bits (shiftL)
import qualified Data.Vector.Algorithms.Radix as R
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as M
import Data.Vector.Random.Mersenne
import Statistics.Function
import Statistics.Sample
import System.Environment
import System.Random.Mersenne.Pure64
-- import Text.PrettyPrint.Mainland
import Text.Printf

import Nikola
import Nikola.Embeddable

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
        (min, max) = (unpairS . minMax) samp

benchmarkIO :: (V.Vector Int -> IO a)
            -> Int
            -> IO a
benchmarkIO f n = do
    g <- liftIO newPureMT
    let v = randoms g n
    f v

radixNikola :: V.Vector Int
            -> IO (V.Vector Int)
radixNikola xs = do
    v <- toCUVector xs
    radixM v
    fromCUVector v

radixM :: CUVector Int
       -> IO ()
radixM xs@(CUVector n _) =
    unsafeWithNewVector n $ \bits -> do
    forM_ [0..31] $ \bit -> do
        bitM (1 `shiftL` bit) xs bits
        splitM xs bits

splitM :: CUVector Int
       -> CUVector Int
       -> IO ()
splitM a@(CUVector n _) flags@(CUVector _ _) =
    unsafeWithNewVector n $ \(nflags :: CUVector Int) ->
    unsafeWithNewVector n $ \(up :: CUVector Int) ->
    unsafeWithNewVector n $ \(index :: CUVector Int) -> do
    let down = nflags
    copyM flags up
    notM flags nflags
    -- dbg "flags" flags
    -- dbg "nflags" nflags
    plusScanM nflags
    -- dbg "down" down
    plusNacsM up
    -- dbg "up" up
    flipM n up up
    -- dbg "up" up
    chooseM flags up down index
    -- dbg "index" index
    permuteIntM a index a

bitM :: Int
     -> CUVector Int
     -> CUVector Int
     -> IO ()
bitM = compile (\b -> mapM (\x -> ((x .&. b) ./=. 0) ? (1, 0)))

permuteIntM :: CUVector Int
            -> CUVector Int
            -> CUVector Int
            -> IO ()
permuteIntM = compile permuteM

plusScanM :: CUVector Int
          -> IO ()
plusScanM xs = do
    scan' xs
  where
    scan' :: CUVector Int -> IO ()
    scan'  (CUVector 0 _)  = return ()
    scan'  xs              = do  sums <- plusBlockedScanM xs
                                 scan' sums
                                 blockedSumM xs sums
                                 unsafeFreeVector sums

plusNacsM :: CUVector Int
          -> IO ()
plusNacsM xs = do
    nacs' xs
  where
    nacs' :: CUVector Int -> IO ()
    nacs'  (CUVector 0 _)  = return ()
    nacs'  xs              = do  sums <- plusBlockedNacsM xs
                                 nacs' sums
                                 blockedSumM xs sums
                                 unsafeFreeVector sums

plusBlockedScanM :: CUVector Int -> IO (CUVector Int)
plusBlockedScanM = compile (blockedScanM (+) 0)

plusBlockedNacsM :: CUVector Int -> IO (CUVector Int)
plusBlockedNacsM = compile (blockedNacsM (+) 0)

blockedSumM :: CUVector Int -> CUVector Int -> IO ()
blockedSumM = compile blockedAddM

copyM :: CUVector Int -> CUVector Int -> IO ()
copyM = compile (mapM id)

notM :: CUVector Int -> CUVector Int -> IO ()
notM = compile (mapM (\x -> (x .==. 0) ? (1, 0)))

flipM :: Int -> CUVector Int -> CUVector Int -> IO ()
flipM = compile (\n -> mapM (\x -> n - 1 - x))

chooseM :: CUVector Int
        -> CUVector Int
        -> CUVector Int
        -> CUVector Int
        -> IO ()
chooseM = compile (zipWith3M (\ x y z -> (x ./=. 0) ? (y, z)))

dbg :: String -> CUVector Int -> IO ()
dbg msg xs = do
    xs' :: [Int] <- fromCUVector xs
    putStrLn $ msg ++ ": " ++ show xs'

radixVector :: V.Vector Int
            -> IO ()
radixVector xs = do
    xs' <- M.new (V.length xs)
    forM_ [0..V.length xs-1] $ \i -> M.write xs' i (xs V.! i)
    R.sort xs'


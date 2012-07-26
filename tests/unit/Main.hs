{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

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

module Main where

import qualified Funs

import Data.Int
import System.Exit (exitFailure, exitSuccess)
import Test.HUnit

import qualified Data.Array.Repa as R
import qualified Data.Array.Repa.Eval as R
import qualified Data.Array.Repa.Repr.CUDA.UnboxedForeign as R

import qualified Data.Vector.Storable as V

import qualified Data.Array.Nikola.Backend.CUDA as N
import qualified Data.Array.Nikola.Backend.CUDA.Haskell as NH
import qualified Data.Array.Nikola.Backend.CUDA.TH as NTH

main :: IO ()
main = do
    count <- runTestTT tests
    case failures count of
      0 -> exitSuccess
      _ -> exitFailure

tests :: Test
tests = TestList [ id_test
                 , map_test
                 , map_inc_test
                 , zip_test
                 , scalar_test
                 , swap_test
                 , th_swap_test
                 , th_scalar_test
                 , th_revmap_test1
                 , th_revmap_test2
                 , th_revmap_test3
                 ]

id_test :: Test
id_test = "id" ~: g 1 ~?= 1
  where
    f :: N.Exp Float -> N.Exp Float
    f x = x

    g :: Float -> Float
    g = NH.compile f

map_test :: Test
map_test = "map" ~:
    g (V.fromList [1..10]) ~?= V.map (+1) (V.fromList [1..10])
  where
    f :: N.Array N.M N.DIM1 (N.Exp Float) -> N.Array N.D N.DIM1 (N.Exp Float)
    f v = N.map (\x -> x + 1) v

    g :: V.Vector Float -> V.Vector Float
    g = NH.compile f

map_inc_test :: Test
map_inc_test = "map inc" ~:
    g (V.fromList [1..10]) ~?= V.map (+1) (V.fromList [1..10])
  where
    f :: N.Array N.M N.DIM1 (N.Exp Int32) -> N.Array N.D N.DIM1 (N.Exp Int32)
    f v = N.map (\x -> x + 1) v

    g :: V.Vector Int32 -> V.Vector Int32
    g = NH.compile f

zip_test :: Test
zip_test = "zip" ~:
    g [1..32] [1..32] ~?= let xs = zipWith (+) [1..32] [1..32]
                          in
                            zipWith (+) xs xs
  where
    f :: N.Array N.M N.DIM1 (N.Exp Float)
      -> N.Array N.M N.DIM1 (N.Exp Float)
      -> N.Array N.D N.DIM1 (N.Exp Float)
    f x y = N.zipWith (+) temp temp
      where
        temp :: N.Array N.D N.DIM1 (N.Exp Float)
        temp = N.zipWith (+) x y

    g :: [Float] -> [Float] -> [Float]
    g = NH.compile f

scalar_test :: Test
scalar_test = "scalar" ~:
    h 10 153 ~?= 2*10 + 2*153
  where
    f :: N.Exp Float -> N.Exp Float -> N.Exp Float
    f x y = g x + g y

    g :: N.Exp Float -> N.Exp Float
    g = N.vapply $ \x -> 2 * x

    h :: Float -> Float -> Float
    h = NH.compile f

swap_test :: Test
swap_test = "swap" ~:
    f xs ~?= map (\(x, y) -> (y, x)) xs
  where
    f :: [(Float, Float)] -> [(Float, Float)]
    f = NH.compile Funs.swap

    xs :: [(Float, Float)]
    xs = [1..10] `zip` [2..11]

th_swap_test :: Test
th_swap_test = "TH swap" ~:
    f xs ~?= map (\(x, y) -> (y, x)) xs
  where
    f :: [(Float, Float)] -> [(Float, Float)]
    f = $(NTH.compileSig Funs.swap (undefined :: [(Float, Float)] -> [(Float, Float)]))

    xs :: [(Float, Float)]
    xs = [1..10] `zip` [2..11]

th_scalar_test :: Test
th_scalar_test = "TH scalar" ~: g 500 ~?= 501
  where
    g :: Float -> Float
    g = $(NTH.compile Funs.inc)

th_revmap_test1 :: Test
th_revmap_test1 = "TH reverse and map (+1) on CF array" ~:
    R.toList (f (R.fromList (R.ix1 (length xs)) xs)) ~?= (reverse . map (+1)) xs
  where
    xs :: [Double]
    xs = [1..10]

    f :: R.Array R.CUF R.DIM1 Double -> R.Array R.CUF R.DIM1 Double
    f = $(NTH.compile Funs.revmapinc)

th_revmap_test2 :: Test
th_revmap_test2 = "TH reverse and map (+1) on list" ~:
    f xs ~?= (reverse . map (+1)) xs
  where
    xs :: [Double]
    xs = [1..10]

    f :: [Double] -> [Double]
    f = $(NTH.compileSig Funs.revmapinc (undefined :: [Double] -> [Double]))

th_revmap_test3 :: Test
th_revmap_test3 = "TH reverse and map (+1) on Vector" ~:
    f xs ~?= (V.reverse . V.map (+1)) xs
  where
    xs :: V.Vector Double
    xs = V.fromList [1..10]

    f :: V.Vector Double -> V.Vector Double
    f = $(NTH.compileSig Funs.revmapinc (undefined :: V.Vector Double -> V.Vector Double))

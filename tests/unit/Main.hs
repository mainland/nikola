{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

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

import qualified Funs

import Data.Int
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit ((@=?))
import Test.QuickCheck

import qualified Data.Array.Repa as R
import qualified Data.Array.Repa.Eval as R
import qualified Data.Array.Repa.Repr.CUDA.UnboxedForeign as R
import qualified Data.Array.Repa.Repr.UnboxedForeign as R

import qualified Data.Vector.Storable as V
import qualified Data.Vector.CUDA.UnboxedForeign as VCUF

import qualified Data.Array.Nikola.Backend.CUDA as N
import qualified Data.Array.Nikola.Backend.CUDA.Haskell as NH
import qualified Data.Array.Nikola.Backend.CUDA.TH as NTH
import qualified Data.Array.Nikola.Combinators as N

type R = Float

type Complex = (R, R)

magnitude :: Complex -> R
magnitude (x,y) = x*x + y*y

instance Num Complex where
    (x,y) + (x',y') = (x+x' ,y+y')
    (x,y) - (x',y') = (x-x', y-y')
    (x,y) * (x',y') = (x*x'-y*y', x*y'+y*x')
    negate (x,y)    = (negate x, negate y)
    abs z           = (magnitude z, 0)
    signum (0,0)    = 0
    signum z@(x,y)  = (x/r, y/r) where r = magnitude z
    fromInteger n   = (fromInteger n, 0)

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [ id_test
        , map_test
        , map_inc_test
        , zip_test
        , scalar_test
        , swap_test
        , iterate_test
        , th_swap_test
        , th_scalar_test
        , th_revmap_test1
        , th_revmap_test2
        , th_revmap_test3
        , th_append_push
        , th_append_delayed
        , th_mandelbrot_init
        -- , testProperty "mandelbrot_init" prop_mandelbrot_init
        , testGroup "QuickCheck index space operations"
                        [ testProperty "reverse" prop_reverse
                        , testProperty "init" prop_init
                        , testProperty "tail" prop_tail
                        , testProperty "take" prop_take
                        , testProperty "drop" prop_drop
                        ]
        ]

id_test :: Test
id_test = testCase "id" $ g 1 @=? 1
  where
    f :: N.Exp Float -> N.Exp Float
    f x = x

    g :: Float -> Float
    g = NH.compile f

map_test :: Test
map_test = testCase "map" $
    g (V.fromList [1..10]) @=? V.map (+1) (V.fromList [1..10])
  where
    f :: N.Array N.G N.DIM1 (N.Exp Float) -> N.Array N.D N.DIM1 (N.Exp Float)
    f v = N.map (\x -> x + 1) v

    g :: V.Vector Float -> V.Vector Float
    g = NH.compile f

map_inc_test :: Test
map_inc_test = testCase "map inc" $
    g (V.fromList [1..10]) @=? V.map (+1) (V.fromList [1..10])
  where
    f :: N.Array N.G N.DIM1 (N.Exp Int32) -> N.Array N.D N.DIM1 (N.Exp Int32)
    f v = N.map (\x -> x + 1) v

    g :: V.Vector Int32 -> V.Vector Int32
    g = NH.compile f

zip_test :: Test
zip_test = testCase "zip" $
    g [1..32] [1..32] @=? let xs = zipWith (+) [1..32] [1..32]
                          in
                            zipWith (+) xs xs
  where
    f :: N.Array N.G N.DIM1 (N.Exp Float)
      -> N.Array N.G N.DIM1 (N.Exp Float)
      -> N.Array N.D N.DIM1 (N.Exp Float)
    f x y = N.zipWith (+) temp temp
      where
        temp :: N.Array N.D N.DIM1 (N.Exp Float)
        temp = N.zipWith (+) x y

    g :: [Float] -> [Float] -> [Float]
    g = NH.compile f

scalar_test :: Test
scalar_test = testCase "scalar" $
    h 10 153 @=? 2*10 + 2*153
  where
    f :: N.Exp Float -> N.Exp Float -> N.Exp Float
    f x y = g x + g y

    g :: N.Exp Float -> N.Exp Float
    g = N.vapply $ \x -> 2 * x

    h :: Float -> Float -> Float
    h = NH.compile f

swap_test :: Test
swap_test = testCase "swap" $
    f xs @=? map (\(x, y) -> (y, x)) xs
  where
    f :: [(Float, Float)] -> [(Float, Float)]
    f = NH.compile Funs.swap

    xs :: [(Float, Float)]
    xs = [1..10] `zip` [2..11]

iterate_test :: Test
iterate_test = testCase "iterate" $
    g (V.fromList [1..10]) @=? V.map (+5) (V.fromList [1..10])
  where
    f :: N.Array N.G N.DIM1 (N.Exp Float) -> N.Array N.D N.DIM1 (N.Exp Float)
    f v = N.map (N.iterate (5 :: N.Exp Int32) (+1)) v

    g :: V.Vector Float -> V.Vector Float
    g = NH.compile f

th_swap_test :: Test
th_swap_test = testCase "TH swap" $
    f xs @=? map (\(x, y) -> (y, x)) xs
  where
    f :: [(Float, Float)] -> [(Float, Float)]
    f = $(NTH.compileSig Funs.swap (undefined :: [(Float, Float)] -> [(Float, Float)]))

    xs :: [(Float, Float)]
    xs = [1..10] `zip` [2..11]

th_scalar_test :: Test
th_scalar_test = testCase "TH scalar" $ g 500 @=? 501
  where
    g :: Float -> Float
    g = $(NTH.compile Funs.inc)

th_revmap_test1 :: Test
th_revmap_test1 = testCase "TH reverse and map (+1) on CF array" $
    R.toList (f (R.fromList (R.ix1 (length xs)) xs)) @=? (reverse . map (+1)) xs
  where
    xs :: [Double]
    xs = [1..10]

    f :: R.Array R.CUF R.DIM1 Double -> R.Array R.CUF R.DIM1 Double
    f = $(NTH.compile Funs.revmapinc)

th_revmap_test2 :: Test
th_revmap_test2 = testCase "TH reverse and map (+1) on list" $
    f xs @=? (reverse . map (+1)) xs
  where
    xs :: [Double]
    xs = [1..10]

    f :: [Double] -> [Double]
    f = $(NTH.compileSig Funs.revmapinc (undefined :: [Double] -> [Double]))

th_revmap_test3 :: Test
th_revmap_test3 = testCase "TH reverse and map (+1) on Vector" $
    f xs @=? (V.reverse . V.map (+1)) xs
  where
    xs :: V.Vector Double
    xs = V.fromList [1..10]

    f :: V.Vector Double -> V.Vector Double
    f = $(NTH.compileSig Funs.revmapinc (undefined :: V.Vector Double -> V.Vector Double))

th_append_push :: Test
th_append_push = testCase "TH append push arrays" $
    f xs @=? V.map (+1) (xs V.++ ys)
  where
    xs :: V.Vector Float
    xs = V.fromList [1..10]

    ys :: V.Vector Float
    ys = V.fromList [0..9]

    f :: V.Vector Float -> V.Vector Float
    f = $(NTH.compileSig Funs.append_push
                         (undefined :: V.Vector Float -> V.Vector Float))

th_append_delayed :: Test
th_append_delayed = testCase "TH append delayed arrays" $
    f xs @=? V.map (+1) (xs V.++ ys)
  where
    xs :: V.Vector Float
    xs = V.fromList [1..10]

    ys :: V.Vector Float
    ys = V.fromList [0..9]

    f :: V.Vector Float -> V.Vector Float
    f = $(NTH.compileSig Funs.append_delayed
                         (undefined :: V.Vector Float -> V.Vector Float))

th_mandelbrot_init :: Test
th_mandelbrot_init = testCase "TH mandelbrot init" $
    f xs xs @=? g xs xs
  where
    xs :: [Complex]
    xs = [1..10] `zip` [2..11]

    f0 :: R.Array R.CUF R.DIM1 Complex
       -> R.Array R.CUF R.DIM1 Complex
       -> R.Array R.CUF R.DIM1 (Complex, Int32)
    f0 = $(NTH.compile Funs.mandelbrot_init)

    f :: [Complex] -> [Complex] -> [(Complex, Int32)]
    f xs ys =
        toHostList $ f0 (fromHostList xs) (fromHostList ys)

    fromHostList :: VCUF.EverywhereUnboxForeign a
                 => [a] -> R.Array R.CUF R.DIM1 a
    fromHostList xs =
        R.fromHostArray $ R.fromListUnboxedForeign (R.ix1 (length xs)) xs

    toHostList :: VCUF.EverywhereUnboxForeign a
               => R.Array R.CUF R.DIM1 a -> [a]
    toHostList = R.toList . R.toHostArray

    g :: [Complex] -> [Complex] -> [(Complex, Int32)]
    g = zipWith (\x y -> (x*y, 0))

prop_reverse :: [Float] -> Property
prop_reverse xs = length xs > 0 ==> reverseN xs == reverse xs
  where
    reverseN :: [Float] -> [Float]
    reverseN = $(NTH.compileSig (N.reverse :: N.Array N.G N.DIM1 (N.Exp Float)
                                           -> N.Array N.D N.DIM1 (N.Exp Float))
                                (undefined :: [Float] -> [Float]))

prop_init :: [Float] -> Property
prop_init xs = length xs > 0 ==> initN xs == init xs
  where
    initN :: [Float] -> [Float]
    initN = $(NTH.compileSig (N.init :: N.Array N.G N.DIM1 (N.Exp Float)
                                     -> N.Array N.D N.DIM1 (N.Exp Float))
                             (undefined :: [Float] -> [Float]))

prop_tail :: [Float] -> Property
prop_tail xs = length xs > 0 ==> tailN xs == tail xs
  where
    tailN :: [Float] -> [Float]
    tailN = $(NTH.compileSig (N.tail :: N.Array N.G N.DIM1 (N.Exp Float)
                                     -> N.Array N.D N.DIM1 (N.Exp Float))
                             (undefined :: [Float] -> [Float]))

prop_take :: Int32 -> [Float] -> Property
prop_take n xs = property $ takeN n xs == take (fromIntegral n) xs
  where
    takeN :: Int32 -> [Float] -> [Float]
    takeN = $(NTH.compileSig (N.take :: N.Exp Int32
                                     -> N.Array N.G N.DIM1 (N.Exp Float)
                                     -> N.Array N.D N.DIM1 (N.Exp Float))
                             (undefined :: Int32 -> [Float] -> [Float]))

prop_drop :: Int32 -> [Float] -> Property
prop_drop n xs = property $ dropN n xs == drop (fromIntegral n) xs
  where
    dropN :: Int32 -> [Float] -> [Float]
    dropN = $(NTH.compileSig (N.drop :: N.Exp Int32
                                     -> N.Array N.G N.DIM1 (N.Exp Float)
                                     -> N.Array N.D N.DIM1 (N.Exp Float))
                             (undefined :: Int32 -> [Float] -> [Float]))

prop_mandelbrot_init :: [Complex] -> [Complex] -> Property
prop_mandelbrot_init xs ys =
    property $ f xs xs == g xs xs
  where
    f0 :: R.Array R.CUF R.DIM1 Complex
       -> R.Array R.CUF R.DIM1 Complex
       -> R.Array R.CUF R.DIM1 (Complex, Int32)
    f0 = $(NTH.compile Funs.mandelbrot_init)

    f :: [Complex] -> [Complex] -> [(Complex, Int32)]
    f xs ys = toHostList $ f0 (fromHostList xs) (fromHostList ys)

    g :: [Complex] -> [Complex] -> [(Complex, Int32)]
    g = zipWith (\x y -> (x*y, 0))

    fromHostList :: VCUF.EverywhereUnboxForeign a
                 => [a] -> R.Array R.CUF R.DIM1 a
    fromHostList xs =
        R.fromHostArray $ R.fromListUnboxedForeign (R.ix1 (length xs)) xs

    toHostList :: VCUF.EverywhereUnboxForeign a
               => R.Array R.CUF R.DIM1 a -> [a]
    toHostList = R.toList . R.toHostArray

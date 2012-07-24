-- Copyright (c) 2010-2102
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

{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Prelude hiding (map, zipWith)
import qualified Prelude as P

import Data.Int
import qualified Data.Vector.Storable as V
import System.Exit (exitFailure, exitSuccess)
import Test.HUnit

import Data.Array.Nikola.Backend.CUDA

main :: IO ()
main = withNewContext $ \_ -> do
    count <- runTestTT tests
    case failures count of
      0 -> exitSuccess
      _ -> exitFailure

tests :: Test
tests = TestList [id_test, map_test, map_int_test
                 ,zip_test, scalar_test]

id_test :: Test
id_test = g 1 ~?= 1
  where
    f :: Exp Float -> Exp Float
    f x = x

    g :: Float -> Float
    g = compile f

map_test :: Test
map_test =
    g (V.fromList [1..10]) ~?= V.map (+1) (V.fromList [1..10])
  where
    f :: Exp (V.Vector Float) -> Exp (V.Vector Float)
    f v = map (\x -> x + 1) v

    g :: V.Vector Float -> V.Vector Float
    g = compile f

map_int_test :: Test
map_int_test =
    g (V.fromList [1..10]) ~?= V.map (+1) (V.fromList [1..10])
  where
    f :: Exp (V.Vector Int32) -> Exp (V.Vector Int32)
    f v = map (\x -> x + 1) v

    g :: V.Vector Int32 -> V.Vector Int32
    g = compile f

zip_test :: Test
zip_test =
    g [1..32] [1..32] ~?= let xs = P.zipWith (+) [1..32] [1..32]
                          in
                            P.zipWith (+) xs xs
  where
    f :: Exp [Float] -> Exp [Float] -> Exp [Float]
    f x y = zipWithPlus temp temp
      where
        zipWithPlus :: Exp [Float] -> Exp [Float] -> Exp [Float]
        zipWithPlus = vapply $ \x y -> zipWith (+) x y

        temp :: Exp [Float]
        temp = zipWithPlus x y

    g :: [Float] -> [Float] -> [Float]
    g = compile f

scalar_test :: Test
scalar_test =
    h 10 153 ~?= 2*10 + 2*153
  where
    f :: Exp Float -> Exp Float -> Exp Float
    f x y = g x + g y

    g :: Exp Float -> Exp Float
    g = vapply $ \x -> 2 * x

    h :: Float -> Float -> Float
    h = compile f

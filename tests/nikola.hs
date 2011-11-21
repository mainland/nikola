{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Prelude hiding (map,
                       zipWith)
import qualified Prelude as P

import CUDA.Context
import Control.Monad
import Data.Packed.Matrix
import Data.Packed.Vector
import Language.C.Quote.CUDA
import qualified Language.C.Syntax as C
import qualified Language.C.Syntax
import Nikola
import Nikola.Compile
import Test.HUnit

main :: IO ()
main = withNewContext $ \_ -> do
    runTestTT tests
    return ()

tests = TestList [embedded_id,
                  embedded_sum,
                  embedded_list_inc,
                  embedded_int_vector_inc,
                  embedded_float_vector_inc,
                  shared_scalar_function,
                  shared_vector_function,
                  plainc_vector_inc,
                  plainc_matmul]

embedded_id = "embedded id" ~: do
    xs <- withCompiledFunction f $ \f -> replicateM 50 $ call f 1
    return (all (== 1) xs)
  where
    f :: Exp Int -> Exp Int
    f x = x

embedded_sum = "embedded sum" ~: do
    x <- withCompiledFunction f $ \f -> call f 10 20 30
    return (x == 60)
  where
    f :: Exp Float -> Exp Float -> Exp Float -> Exp Float
    f x y z = x + y + z

embedded_list_inc = "embedded list increment" ~: do
    xs <- withCompiledFunction f $ \f -> call f [1..512]
    return (xs == [2..513])
  where
    f :: Exp [Int] -> Exp [Int]
    f = map (\x -> x + 1)

embedded_int_vector_inc = "embedded int vector increment" ~: do
    xs <- withCompiledFunction f $ \f -> call f (fromList [1..512])
    return (toList xs == [2..513])
  where
    f :: Exp (Vector Int) -> Exp (Vector Int)
    f = map (\x -> x + 1)

embedded_float_vector_inc = "embedded float vector increment" ~: do
    xs <- withCompiledFunction f $ \f -> call f (fromList [1..512])
    return (toList xs == [2..513])
  where
    f :: Exp (Vector Float) -> Exp (Vector Float)
    f = map (\x -> x + 1)

shared_scalar_function = "shared scalar function" ~: do
    x <- withCompiledFunction f $ \f -> call f 2 3
    return (x == 10)
  where
    f :: Exp Float -> Exp Float -> Exp Float
    f x y = g x + g y

    g :: Exp Float -> Exp Float
    g = vapply $ \x -> 2 * x

shared_vector_function = "shared vector function" ~: do
    xs <- withCompiledFunction f $ \f -> call f [1..32] [1..32]
    return (xs == P.map (* 4) [1..32])
  where
    f :: Exp [Float] -> Exp [Float] -> Exp [Float]
    f x y = zipWithPlus temp temp
      where
        zipWithPlus :: Exp [Float] -> Exp [Float] -> Exp [Float]
        zipWithPlus = vapply $ \x y -> zipWith (+) x y

        temp :: Exp [Float]
        temp = zipWithPlus x y

plainc_vector_inc = "plain C vector increment" ~: do
    xs <- withCompiledCFun f $ \f ->
          call f [1.0..512.0]
    return (xs == [2.0..513.0])
  where
    f :: CFun (Exp [Float] -> Exp [Float])
    f = CFun { cfunName = "f"
             , cfunDefs = defs
             , cfunAllocs = [vectorT FloatT 0]
             , cfunExecConfig = ExecConfig { gridDimX  = nGridDimX (NVecLength 0) 64
                                           , gridDimY  = 1
                                           , blockDimX = 64
                                           , blockDimY = 1
                                           , blockDimZ = 1
                                           }
             }
      where
        defs :: [C.Definition]
        defs = [$cunit|
          extern "C" __global__ void f(float* x, unsigned int n, float* result, long long* n2)
          {
              const int i = blockIdx.x * 64 + threadIdx.x;

              result[i] = x[i] + 1;

              if (i == 0)
                 *n2 = n;
          }
          |]

plainc_matmul = "plain C matrix multiply" ~: do
    let a = buildMatrix n n $ \(r, c) ->
            (fromIntegral r)*2.0 + (fromIntegral c)*2.0
    let b = ident n
    c <- withCompiledCFun matmul $ \f ->
         call f a b
    return (toLists c == toLists a)
  where
    n :: Int
    n = 128

    matmul :: CFun (Exp (Matrix Float) -> Exp (Matrix Float) -> Exp (Matrix Float))
    matmul = CFun { cfunName = "matmul"
                  , cfunDefs = defs
                  , cfunAllocs = [matrixT FloatT 1 0 1]
                  , cfunExecConfig = ExecConfig { gridDimX  = 1
                                                , gridDimY  = 1
                                                , blockDimX = 1
                                                , blockDimY = 1
                                                , blockDimZ = 1
                                                }
                  }
      where
        defs :: [C.Definition]
        defs = [$cunit|
          extern "C" __global__ void matmul(float* A, int ap, int ar, int ac,
                                            float* B, int bp, int br, int bc,
                                            float* C)
          {
              for (int r = 0; r < ar; ++r) {
                  for (int c = 0; c < bc; ++c) {
                      float sum = 0.0;

                      for (int i = 0; i < ac; ++i)
                        sum += A[r*ap+i] * B[i*bp+c];

                      C[r*bp+c] = sum;
                  }
              }
          }
          |]

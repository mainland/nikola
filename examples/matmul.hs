{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import CUDA.Context
import Control.Monad
import Data.Array.Vector
import Data.Packed.Matrix
import Language.C.Quote.CUDA
import qualified Language.C.Syntax as C
import qualified Language.C.Syntax
import Nikola hiding (map)
import Nikola.Compile
import Numeric.LinearAlgebra.Algorithms
import Statistics.Sample
import System.IO
import Text.Printf
import Text.PrettyPrint.Mainland

main :: IO ()
main = withNewContext $ \_ -> do
    -- check "dumbk" dumbk

    check "k1 32" (k1 32)
    check "k1 64" (k1 64)
    check "k1 128" (k1 128)
    check "k1 256" (k1 256)
    check "k1 512" (k1 512)

    check "k2 32" (k2 32)
    check "k2 64" (k2 64)
    check "k2 128" (k2 128)
    check "k2 256" (k2 256)
    check "k2 512" (k2 512)
  where
    n :: Int
    n = 512

    a :: Element a => Matrix a
    a = buildMatrix n n $ \(i, j) ->
        (fromIntegral i)*2.0 + (fromIntegral j)*2.0

    b :: Element a => Matrix a
    b = buildMatrix n n $ \(i, j) ->
        if i == j - 1 then
            -1.0
        else if i == j + 1 then
            2.0
        else
            0.0

    c :: [[Float]]
    c = (map (map realToFrac) . toLists) (a `multiply` b :: Matrix Double)

    check :: String -> CFun (Exp (Matrix Float) -> Exp (Matrix Float) -> Exp (Matrix Float)) -> IO ()
    check desc k = do
        withFile (desc ++ ".cu") WriteMode $ \h ->
            hPutStr h $ (show . stack . map ppr . cfunDefs) k
        samples <- withCompiledCFun k $ \f ->
                   replicateM 5 $ do
                   timeKernel f $ \f -> do
                   call f a b
        when (not (all (== c) (map (toLists . snd) samples))) $
            fail "Incorrect kernel"
        let gflops = toU (map (toGFlops . fst) samples)
        printf "%s: %0.2f+/-%0.2f GFlops\n" desc (mean gflops) (stdDev gflops)
      where
        toGFlops :: Double -> Double
        toGFlops t = fromIntegral (n*n*n*2) / realToFrac t / 1e9

dumbk :: CFun (Exp (Matrix Float) -> Exp (Matrix Float) -> Exp (Matrix Float))
dumbk = CFun { cfunName = "k"
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
      extern "C" __global__ void k(float* a, int lda, int m, int k,
                                   float* b, int ldb, int k_, int n,
                                   float* c)
      {
          for (int i = 0; i < m; ++i) {
              for (int j = 0; j < n; ++j) {
                  float sum = 0.0;

                  for (int l = 0; l < k; ++l)
                      sum += a[i+l*lda] * b[l+j*ldb];

                  c[i+j*ldb] = sum;
              }
          }
      }
      |]

-- Multiply an 'm x l' matrix by an 'l x n' matrix to yield an 'm x n'
-- matrix. 'w' is the number of threads (the width of the SIMD block).
k1 :: Int -> CFun (Exp (Matrix Float) -> Exp (Matrix Float) -> Exp (Matrix Float))
k1 w = CFun { cfunName = "k"
            , cfunDefs = defs
            , cfunAllocs = [matrixT FloatT 1 0 1]
            , cfunExecConfig = ExecConfig { gridDimX  = nGridDimX (NMatRows 0) w
                                          , gridDimY  = (NMatCols 1)
                                          , blockDimX = w
                                          , blockDimY = 1
                                          , blockDimZ = 1
                                          }
            }
  where
    defs :: [C.Definition]
    defs = [$cunit|
      extern "C" __global__ void k(float* a, int a_pitch, int m, int l,
                                   float* b, int b_pitch, int l_, int n,
                                   float* c)
      {
          int tid = threadIdx.x;
          int i = blockIdx.x*$int:w + tid;
          int j = blockIdx.y;

          float sum = 0.0;

          for (int k = 0; k < l; ++k)
            sum += a[i+k*a_pitch] * b[k+j*b_pitch];

          c[i+j*b_pitch] = sum;
      }
      |]

-- Multiply an 'm x l' matrix by an 'l x n' matrix to yield an 'm x n'
-- matrix. 'w' is the number of threads (the width of the SIMD block).
k2 :: Int -> CFun (Exp (Matrix Float) -> Exp (Matrix Float) -> Exp (Matrix Float))
k2 w = CFun { cfunName = "k"
            , cfunDefs = defs
            , cfunAllocs = [matrixT FloatT 1 0 1]
            , cfunExecConfig = ExecConfig { gridDimX  = nGridDimX (NMatRows 0) w
                                          , gridDimY  = (NMatCols 1)
                                          , blockDimX = w
                                          , blockDimY = 1
                                          , blockDimZ = 1
                                          }
            }
  where
    defs :: [C.Definition]
    defs = [$cunit|
      extern "C" __global__ void k(float* a, int a_pitch, int m, int l,
                                   float* b, int b_pitch, int l_, int n,
                                   float* c)
      {
          int tid = threadIdx.x;
          int i = blockIdx.x*$int:w + tid;
          int j = blockIdx.y;
          __shared__ float bbuf[$int:w];

          float sum = 0.0;

          for (int ks = 0; ks < l; ks += $int:w) {
              bbuf[tid] = b[ks + tid + j*b_pitch];
              __syncthreads();

              for (int k = ks; k < ks+$int:w; ++k)
                  sum += a[i+k*a_pitch] * bbuf[k-ks];
              __syncthreads();
          }

          c[i+j*b_pitch] = sum;
      }
      |]

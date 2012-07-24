{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Prelude hiding (map)

import qualified Data.Vector.Storable as V
import Language.C.Quote.CUDA
import qualified Language.C.Syntax as C

#if !MIN_VERSION_template_haskell(2,7,0)
import qualified Data.Loc
import qualified Data.Symbol
import qualified Language.C.Syntax
#endif /* !MIN_VERSION_template_haskell(2,7,0) */

import Data.Array.Nikola.Backend.CUDA
import Data.Array.Nikola.Backend.CUDA.CodeGen

main :: IO ()
main = withNewContext $ \_ -> do
    test

test :: IO ()
test = do
    print (g v)
  where
    g :: V.Vector Float -> V.Vector Float
    g = compile f2

    v :: V.Vector Float
    v = V.fromList [0..31]

f :: Exp (V.Vector Float) -> Exp (V.Vector Float)
f = map inc

inc :: Exp Float -> Exp Float
inc = vapply $ \x -> x + 1

f2 :: CFun (Exp (V.Vector Float) -> Exp (V.Vector Float))
f2 = CFun { cfunName = "f2"
          , cfunDefs = defs
          , cfunAllocs = [vectorArgT FloatT (ParamIdx 0)]
          , cfunExecConfig = ExecConfig { gridDimX  = fromIntegral 240
                                        , gridDimY  = 1
                                        , blockDimX = fromIntegral 128
                                        , blockDimY = 1
                                        , blockDimZ = 1
                                        }
          }
  where
    defs :: [C.Definition]
    defs = [cunit|
__device__ float f0(float x2)
{
    float v4;

    v4 = x2 + 1.0F;
    return v4;
}
extern "C" __global__ void f2(float* x0, int x0n, float* temp, int* tempn)
{
    for (int i = (blockIdx.x + blockIdx.y * gridDim.x) * 128 + threadIdx.x; i <
         x0n; i += 128 * 240) {
        if (i < x0n) {
            {
                float temp0;

                temp0 = f0(x0[i]);
                temp[i] = temp0;
            }
            if (i == 0)
                *tempn = x0n;

        }
    }
    __syncthreads();
}
      |]

{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}

module BlackScholes.CUDA (
    blackscholes
  ) where

import qualified Data.Vector.Storable as V
import Language.C.Quote.CUDA
import qualified Language.C.Syntax as C

#if !MIN_VERSION_template_haskell(2,7,0)
import qualified Data.Loc
import qualified Data.Symbol
import qualified Language.C.Syntax
#endif /* !MIN_VERSION_template_haskell(2,7,0) */

import Nikola

blackscholes :: CFun (   Exp (V.Vector Float)
                      -> Exp (V.Vector Float)
                      -> Exp (V.Vector Float)
                      -> Exp Float
                      -> Exp Float
                      -> Exp (V.Vector Float)
                     )
blackscholes = CFun { cfunName = "blackscholes"
                    , cfunDefs = defs
                    , cfunAllocs = [vectorT FloatT nmin]
                    , cfunExecConfig = ExecConfig { gridDimX  = fromIntegral 480
                                                  , gridDimY  = 1
                                                  , blockDimX = fromIntegral 128
                                                  , blockDimY = 1
                                                  , blockDimZ = 1
                                                  }
                    }
  where
    defs :: [C.Definition]
    defs = [cunit|
      __device__ inline float cndGPU(float d){
          const float       A1 = 0.31938153f;
          const float       A2 = -0.356563782f;
          const float       A3 = 1.781477937f;
          const float       A4 = -1.821255978f;
          const float       A5 = 1.330274429f;
          const float RSQRT2PI = 0.39894228040143267793994605993438f;

          float
              K = 1.0f / (1.0f + 0.2316419f * fabsf(d));

          float
              cnd = RSQRT2PI * __expf(- 0.5f * d * d) *
              (K * (A1 + K * (A2 + K * (A3 + K * (A4 + K * A5)))));

          if(d > 0)
              cnd = 1.0f - cnd;

          return cnd;
      }

      __device__ inline void BlackScholesBodyGPU(
          float* CallResult,
          float S, //Stock price
          float X, //Option strike
          float T, //Option years
          float R, //Riskless rate
          float V  //Volatility rate
      ){
          float sqrtT, expRT;
          float d1, d2, CNDD1, CNDD2;

          sqrtT = sqrtf(T);
          d1 = (__logf(S / X) + (R + 0.5f * V * V) * T) / (V * sqrtT);
          d2 = d1 - V * sqrtT;

          CNDD1 = cndGPU(d1);
          CNDD2 = cndGPU(d2);

          expRT = __expf(- R * T);
          *CallResult = S * CNDD1 - X * expRT * CNDD2;
      }

      extern "C" __global__ void blackscholes(float* ss, int sn,
                                              float* xs, int xn,
                                              float* ts, int tn,
                                              float r,
                                              float v,
                                              float* call,
                                              long long* calln)
      {
        const int optN = sn < xn ? (sn < tn ? sn : tn) : (xn < tn ? xn : tn);
        const int tid = blockDim.x * blockIdx.x + threadIdx.x;
        const int THREAD_N = blockDim.x * gridDim.x;

        for(int opt = tid; opt < optN; opt += THREAD_N)
           BlackScholesBodyGPU(&(call[opt]), ss[opt], xs[opt], ts[opt], r, v);

        *calln = optN;
      }
      |]

    nmin :: N
    nmin = NMin [NDim 0 (ParamIdx 0),
                 NDim 0 (ParamIdx 1),
                 NDim 0 (ParamIdx 2)]

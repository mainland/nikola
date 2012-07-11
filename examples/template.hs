{-# LANGUAGE QuasiQuotes #-}

module Main where

import Language.C.Quote.CUDA
import qualified Language.C.Syntax as C
import qualified Language.C.Syntax

foo t n = [$cfun|
__global__ void MatrixMulKernel($ty:t *A, $ty:t *B, $ty:t *C)
{
    // 2D Thread ID (assuming that only *one* block will be executed)
    int x = threadIdx.x;
    int y = threadIdx.y;

    // tmp is used to store the element of the matrix that is computed by the
    // thread
    $ty:t tmp = 0;

    // Each thread loads one row of A and one column of B,
    // to produce one element of C.
    for (int i = 0; i < $int:n; ++i) {
        $ty:t a = A[y * $int:n + i];
        $ty:t b = B[i * $int:n + x];
        tmp += a * b;
    }

    // Write the matrix to device memory; each thread writes one element
    C[y * $int:n + x] = tmp;
}
|]

main :: IO ()
main = do
    print $ foo [$cty|float|] 10
    print $ foo [$cty|int|] 100

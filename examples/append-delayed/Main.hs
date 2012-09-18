module Main where

import Prelude hiding ((++), map, replicate, reverse)

import Data.Array.Nikola.Backend.CUDA

main :: IO ()
main = defaultMain f

f :: Array M DIM1 (Exp Float)
  -> Array D DIM1 (Exp Float)
f v1 =
      map (+1) (append v1 v2)
  where
    v2 :: Array D DIM1 (Exp Float)
    v2 = enumFromN (ix1 10) 0

module Main where

import Prelude hiding ((++), map, replicate, reverse)

import Data.Array.Nikola.Backend.CUDA

main :: IO ()
main = defaultMain f

f :: Array M DIM1 (Exp Float) -> Array PSH DIM1 (Exp Float)
f v1 = mapP (+1) (appendP (push v1) v2)
  where
    v2 :: Array PSH DIM1 (Exp Float)
    v2 = push (range 0 9)

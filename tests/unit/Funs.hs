module Funs where

import Prelude hiding (map, reverse)

import Data.Array.Nikola.Backend.CUDA

revmapinc :: Array M DIM1 (Exp Double) -> Array D DIM1 (Exp Double)
revmapinc = reverse . map (+1)

inc :: Exp Float -> Exp Float
inc = (+1)

swap :: Array M DIM1 (Exp Float, Exp Float)
     -> Array D DIM1 (Exp Float, Exp Float)
swap = map (\(x, y) -> (y, x))

append_push :: Array M DIM1 (Exp Float) -> Array PSH DIM1 (Exp Float)
append_push v1 =
    mapP (+1) (appendP (push v1) v2)
  where
    v2 :: Array PSH DIM1 (Exp Float)
    v2 = push (enumFromN (ix1 10) 0)

append_delayed :: Array M DIM1 (Exp Float) -> Array D DIM1 (Exp Float)
append_delayed v1 =
    map (+1) (append v1 v2)
  where
    v2 :: Array D DIM1 (Exp Float)
    v2 = enumFromN (ix1 10) 0

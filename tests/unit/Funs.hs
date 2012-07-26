module Funs where

import qualified Data.Array.Nikola.Backend.CUDA as N

revmapinc :: N.Array N.M N.DIM1 (N.Exp Double) -> N.Array N.D N.DIM1 (N.Exp Double)
revmapinc = N.reverse . N.map (+1)

inc :: N.Exp Float -> N.Exp Float
inc = (+1)

swap :: N.Array N.M N.DIM1 (N.Exp Float, N.Exp Float)
      -> N.Array N.D N.DIM1 (N.Exp Float, N.Exp Float)
swap = N.map (\(x, y) -> (y, x))

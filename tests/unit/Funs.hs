{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- |
-- Module      : Data.Array.Nikola.Util.Random
-- Copyright   : (c) Geoffrey Mainland 2012
-- License     : BSD-style
--
-- Maintainer  : Geoffrey Mainland <mainland@apeiron.net>
-- Stability   : experimental
-- Portability : non-portable

module Funs where

import Prelude hiding (map, reverse, zipWith)

import Data.Array.Nikola.Backend.CUDA
import Data.Int

type R = Float

type Complex = (Exp R, Exp R)

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

magnitude :: Complex -> Exp R
magnitude (x,y) = x*x + y*y

instance Num Complex where
    (x,y) + (x',y') = (x+x' ,y+y')
    (x,y) - (x',y') = (x-x', y-y')
    (x,y) * (x',y') = (x*x'-y*y', x*y'+y*x')
    negate (x,y)    = (negate x, negate y)
    abs z           = (magnitude z, 0)
    signum (0,0)    = 0
    signum z@(x,y)  = (x/r, y/r) where r = magnitude z
    fromInteger n   = (fromInteger n, 0)

mandelbrot_init :: Array M DIM1 Complex
                -> Array M DIM1 Complex
                -> Array D DIM1 (Complex, Exp Int32)
mandelbrot_init = zipWith (\x y -> (x*y, 0))

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Mandel where

import qualified Prelude as P
import Prelude hiding (map, zipWith)
import Prelude (Num)

import Data.Int

import Data.Array.Nikola.Backend.CUDA

type F = Float

type Complex = (Exp F, Exp F)

magnitude :: Complex -> Exp F
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

type ComplexPlane r = Array r DIM2 Complex

type StepPlane r = Array r DIM2 (Complex, Exp Int32)

step :: ComplexPlane M -> StepPlane M -> StepPlane D
step cs = zipWith stepPoint cs
  where
    stepPoint :: Complex -> (Complex, Exp Int32) -> (Complex, Exp Int32)
    stepPoint c (z,i) =
        if magnitude z' >* 4.0
        then (z, i)
        else (z', i+1)
      where
         z' = next c z

next :: Complex -> Complex -> Complex
next c z = c + (z * z)

genPlane :: Exp F
         -> Exp F
         -> Exp F
         -> Exp F
         -> Exp Int32
         -> Exp Int32
         -> ComplexPlane D
genPlane lowx lowy highx highy viewx viewy =
    fromFunction (Z:.viewy:.viewx) $ \(Z:.x:.y) ->
        (lowx + (fromInt x*xsize)/fromInt viewx, lowy + (fromInt y*ysize)/fromInt viewy)
   where
      xsize, ysize :: Exp F
      xsize = highx - lowx
      ysize = highy - lowy

mkinit :: ComplexPlane M -> StepPlane D
mkinit cs = map f cs
  where
    f :: Complex -> (Complex, Exp Int32)
    f z = (z,0)

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Mandelbrot.NikolaV2.Implementation where

import qualified Prelude as P
import Prelude hiding (iterate, map, zipWith)

import Data.Array.Nikola.Backend.CUDA
import Data.Array.Nikola.Combinators
import Data.Array.Nikola.Eval
import Data.Int

import Mandelbrot.Types hiding (Bitmap, Complex, ComplexPlane, StepPlane)

type Bitmap r = Array r DIM2 (Exp RGBA)

type Complex = (Exp R, Exp R)

type ComplexPlane r = Array r DIM2 Complex

type StepPlane r = Array r DIM2 (Complex, Exp Int32)

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

stepN :: Exp I -> ComplexPlane M -> StepPlane M -> P (StepPlane M)
stepN n cs zs =
    computeP $ zipWith stepPoint cs zs
  where
    stepPoint :: Complex -> (Complex, Exp Int32) -> (Complex, Exp Int32)
    stepPoint c (z,i) = iterate n go (z,i)
      where
        go :: (Complex, Exp Int32) -> (Complex, Exp Int32)
        go (z,i) =
            if magnitude z' >* 4.0
            then (z, i)
            else (z', i+1)
          where
             z' = next c z

    next :: Complex -> Complex -> Complex
    next c z = c + (z * z)

genPlane :: Exp R
         -> Exp R
         -> Exp R
         -> Exp R
         -> Exp Int32
         -> Exp Int32
         -> P (ComplexPlane M)
genPlane lowx lowy highx highy viewx viewy =
    computeP $
    fromFunction (Z:.viewy:.viewx) $ \(Z:.y:.x) ->
        (lowx + (fromInt x*xsize)/fromInt viewx, lowy + (fromInt y*ysize)/fromInt viewy)
   where
      xsize, ysize :: Exp R
      xsize = highx - lowx
      ysize = highy - lowy

mkinit :: ComplexPlane M -> P (StepPlane M)
mkinit cs = computeP $ map f cs
  where
    f :: Complex -> (Complex, Exp Int32)
    f z = (z,0)

prettyRGBA :: Exp Int32 -> (Complex, Exp Int32) -> Exp RGBA
{-# INLINE prettyRGBA #-}
prettyRGBA limit (_, s) = r + g + b + a
  where
    t = fromInt $ ((limit - s) * 255) `quot` limit
    r = (t     `mod` 128 + 64) * 0x1000000
    g = (t * 2 `mod` 128 + 64) * 0x10000
    b = (t * 3 `mod` 256     ) * 0x100
    a = 0xFF

prettyMandelbrot :: Exp Int32 -> StepPlane M -> P (Bitmap M)
prettyMandelbrot limit zs = computeP $ map (prettyRGBA limit) zs

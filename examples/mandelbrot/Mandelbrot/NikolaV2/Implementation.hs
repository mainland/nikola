{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Mandelbrot.NikolaV2.Implementation where

import qualified Prelude as P
import Prelude hiding (map, zipWith)

import Data.Array.Nikola.Backend.CUDA
import Data.Array.Nikola.Eval
import Data.Int
import Data.Word

type R = Double

type RGBA = Word32

type Bitmap r = Array r DIM2 (Exp RGBA)

type Complex = (Exp R, Exp R)

type MComplexPlane r = MArray r DIM2 Complex

type ComplexPlane r = Array r DIM2 Complex

type StepPlane r = Array r DIM2 (Complex, Exp Int32)

type MStepPlane r = MArray r DIM2 (Complex, Exp Int32)

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

step :: ComplexPlane G -> MStepPlane G -> P ()
step cs mzs = do
    zs <- unsafeFreezeMArray mzs
    loadP (zipWith stepPoint cs zs) mzs
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

genPlane :: Exp R
         -> Exp R
         -> Exp R
         -> Exp R
         -> Exp Int32
         -> Exp Int32
         -> MComplexPlane G
         -> P ()
genPlane lowx lowy highx highy viewx viewy mcs =
    flip loadP mcs $
    fromFunction (Z:.viewy:.viewx) $ \(Z:.y:.x) ->
        (lowx + (fromInt x*xsize)/fromInt viewx, lowy + (fromInt y*ysize)/fromInt viewy)
   where
      xsize, ysize :: Exp R
      xsize = highx - lowx
      ysize = highy - lowy

mkinit :: ComplexPlane G -> MStepPlane G -> P ()
mkinit cs mzs = loadP (map f cs) mzs
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

prettyMandelbrot :: Exp Int32 -> StepPlane G -> P (Bitmap G)
prettyMandelbrot limit zs = computeP $ map (prettyRGBA limit) zs

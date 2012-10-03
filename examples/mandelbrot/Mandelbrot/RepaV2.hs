{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Mandelbrot.RepaV2
    ( mandelbrot
    , prettyMandelbrot
    ) where

import Prelude hiding (map, zipWith)
import qualified Prelude as P

import Data.Array.Repa
import Data.Array.Repa.Repr.ForeignPtr

import Mandelbrot.Types

magnitude :: Complex -> R
magnitude (x,y) = x*x + y*y

instance Num Complex where
    {-# INLINE (+) #-}
    {-# INLINE (-) #-}
    {-# INLINE (*) #-}
    {-# INLINE negate #-}
    {-# INLINE abs #-}
    {-# INLINE signum #-}
    {-# INLINE fromInteger #-}
    (x,y) + (x',y') = (x+x', y+y')
    (x,y) - (x',y') = (x-x', y-y')
    (x,y) * (x',y') = (x*x'-y*y', x*y'+y*x')
    negate (x,y)    = (negate x, negate y)
    abs z           = (magnitude z, 0)
    signum (0,0)    = 0
    signum z@(x,y)  = (x/r, y/r) where r = magnitude z
    fromInteger n   = (fromInteger n, 0)

stepN :: I -> ComplexPlane U -> StepPlane U -> IO (StepPlane U)
stepN n cs zs =
    computeP $ zipWith (stepPoint n) cs zs
  where
    stepPoint :: I -> Complex -> (Complex, I) -> (Complex, I)
    {-# INLINE stepPoint #-}
    stepPoint 0 !_ (!z,!i) =
        (z, i)

    stepPoint !k !c (!z,!i) =
        if magnitude z' > 4.0
        then (z, i)
        else stepPoint (k-1) c (z', i+1)
      where
         z' = next c z

    next :: Complex -> Complex -> Complex
    {-# INLINE next #-}
    next !c !z = c + (z * z)

genPlane :: R
         -> R
         -> R
         -> R
         -> I
         -> I
         -> ComplexPlane D
genPlane lowx lowy highx highy viewx viewy =
    fromFunction (Z:.fromIntegral viewy:.fromIntegral viewx) $ \(Z:.(!y):.(!x)) ->
        (lowx + (fromIntegral x*xsize)/fromIntegral viewx,
         lowy + (fromIntegral y*ysize)/fromIntegral viewy)
   where
      xsize, ysize :: R
      xsize = highx - lowx
      ysize = highy - lowy

mkinit :: ComplexPlane U -> StepPlane D
mkinit cs = map f cs
  where
    f :: Complex -> (Complex, I)
    {-# INLINE f #-}
    f z = (z, 0)

mandelbrot :: R
           -> R
           -> R
           -> R
           -> I
           -> I
           -> I
           -> IO (StepPlane U)
mandelbrot lowx lowy highx highy viewx viewy depth = do
    cs  <- computeP $ genPlane lowx lowy highx highy viewx viewy
    zs0 <- computeP $ mkinit cs
    stepN depth cs zs0

prettyRGBA :: I -> (Complex, I) -> RGBA
{-# INLINE prettyRGBA #-}
prettyRGBA limit (_, s) = r + g + b + a
  where
    t = fromIntegral $ ((limit - s) * 255) `quot` limit
    r = (t     `mod` 128 + 64) * 0x1000000
    g = (t * 2 `mod` 128 + 64) * 0x10000
    b = (t * 3 `mod` 256     ) * 0x100
    a = 0xFF

prettyMandelbrot :: I -> StepPlane U -> IO (Bitmap F)
prettyMandelbrot limit zs = computeP $ map (prettyRGBA limit) zs

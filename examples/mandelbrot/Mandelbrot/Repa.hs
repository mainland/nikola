{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Mandelbrot.Repa
    ( mandelbrot
    , prettyMandelbrot
    ) where

import Prelude hiding (map, zipWith)
import qualified Prelude as P

import Data.Array.Repa
import Data.Array.Repa.Repr.ForeignPtr
import Data.Word

type FloatRep = Float

type Complex = (FloatRep, FloatRep)

magnitude :: Complex -> FloatRep
magnitude (x,y) = x*x + y*y

instance Num Complex where
    {-# INLINE (+) #-}
    {-# INLINE (-) #-}
    {-# INLINE (*) #-}
    {-# INLINE negate #-}
    {-# INLINE abs #-}
    {-# INLINE signum #-}
    {-# INLINE fromInteger #-}
    (x,y) + (x',y') = (x+x' ,y+y')
    (x,y) - (x',y') = (x-x', y-y')
    (x,y) * (x',y') = (x*x'-y*y', x*y'+y*x')
    negate (x,y)    = (negate x, negate y)
    abs z           = (magnitude z, 0)
    signum (0,0)    = 0
    signum z@(x,y)  = (x/r, y/r) where r = magnitude z
    fromInteger n   = (fromInteger n, 0)

type ComplexPlane r = Array r DIM2 Complex

type StepPlane r = Array r DIM2 (Complex, Int)

step :: Source r Complex
     => ComplexPlane r -> StepPlane U -> IO (StepPlane U)
step cs zs =
    computeP $ zipWith stepPoint cs zs
  where
    stepPoint :: Complex -> (Complex, Int) -> (Complex, Int)
    {-# INLINE stepPoint #-}
    stepPoint !c (!z,!i) =
        if magnitude z' > 4.0
        then (z, i)
        else (z', i+1)
      where
         z' = next c z

next :: Complex -> Complex -> Complex
{-# INLINE next #-}
next !c !z = c + (z * z)

genPlane :: FloatRep
         -> FloatRep
         -> FloatRep
         -> FloatRep
         -> Int
         -> Int
         -> ComplexPlane D
genPlane lowx lowy highx highy viewx viewy =
    fromFunction (Z:.viewy:.viewx) $ \(Z:.(!x):.(!y)) ->
        (lowx + (fromIntegral x*xsize)/fromIntegral viewx,
         lowy + (fromIntegral y*ysize)/fromIntegral viewy)
   where
      xsize, ysize :: FloatRep
      xsize = highx - lowx
      ysize = highy - lowy

mkinit :: Source r Complex => ComplexPlane r -> StepPlane D
mkinit cs = map f cs
  where
    f :: Complex -> (Complex, Int)
    {-# INLINE f #-}
    f z = (z, 0)

mandelbrot :: FloatRep
           -> FloatRep
           -> FloatRep
           -> FloatRep
           -> Int
           -> Int
           -> Int
           -> IO (StepPlane U)
mandelbrot lowx lowy highx highy viewx viewy depth =
    computeP zs0 >>= iterateM (step cs) depth
  where
    cs :: ComplexPlane D
    cs = genPlane lowx lowy highx highy viewx viewy

    zs0 :: StepPlane D
    zs0 = mkinit cs

iterateM :: Monad m => (a -> m a) -> Int -> a -> m a
iterateM f = go
  where
    go 0 x = return x
    go n x = f x >>= go (n-1)

type RGBA = Word32

prettyRGBA :: Int -> (Complex, Int) -> RGBA
{-# INLINE prettyRGBA #-}
prettyRGBA limit (_, s) = r + g + b + a
  where
    t = fromIntegral $ ((limit - s) * 255) `quot` limit
    r = (t     `mod` 128 + 64) * 0x1000000
    g = (t * 2 `mod` 128 + 64) * 0x10000
    b = (t * 3 `mod` 256     ) * 0x100
    a = 0xFF

prettyMandelbrot :: Int -> StepPlane U -> IO (Array F DIM2 RGBA)
prettyMandelbrot limit zs = computeP $ map (prettyRGBA limit) zs

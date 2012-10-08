{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Mandelbrot.RepaV3 (mandelbrotImageGenerator) where

import Prelude hiding (map, zipWith)
import qualified Prelude as P

import Data.Array.Repa
import Data.Array.Repa.Eval
import Data.Array.Repa.Repr.ForeignPtr
import Data.Array.Repa.Repr.HintInterleave
import Data.IORef
import Data.Word

type R = Double

type Complex = (R, R)

type RGBA = Word32

type Bitmap r = Array r DIM2 RGBA

type MBitmap r = MVec r RGBA

type ComplexPlane r = Array r DIM2 Complex

type StepPlane r = Array r DIM2 (Complex, Int)

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

stepN :: Int -> ComplexPlane U -> StepPlane U -> IO (StepPlane U)
stepN n cs zs =
    computeP $ hintInterleave $ zipWith (stepPoint n) cs zs
  where
    stepPoint :: Int -> Complex -> (Complex, Int) -> (Complex, Int)
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
         -> Int
         -> Int
         -> ComplexPlane D
genPlane lowx lowy highx highy viewx viewy =
    fromFunction (Z:.viewy:.viewx) $ \(Z:.(!y):.(!x)) ->
        (lowx + (fromIntegral x*xsize)/fromIntegral viewx,
         lowy + (fromIntegral y*ysize)/fromIntegral viewy)
   where
      xsize, ysize :: R
      xsize = highx - lowx
      ysize = highy - lowy

mkinit :: Source r Complex => ComplexPlane r -> StepPlane D
mkinit cs = map f cs
  where
    f :: Complex -> (Complex, Int)
    {-# INLINE f #-}
    f z = (z, 0)

mandelbrot :: R
           -> R
           -> R
           -> R
           -> Int
           -> Int
           -> Int
           -> IO (StepPlane U)
mandelbrot lowx lowy highx highy viewx viewy depth = do
    cs  <- computeP $ genPlane lowx lowy highx highy viewx viewy
    zs0 <- computeP $ mkinit cs
    stepN depth cs zs0

prettyRGBA :: Int -> (Complex, Int) -> RGBA
{-# INLINE prettyRGBA #-}
prettyRGBA limit (_, s) = r + g + b + a
  where
    t = fromIntegral $ ((limit - s) * 255) `quot` limit
    r = (t     `mod` 128 + 64) * 0x1000000
    g = (t * 2 `mod` 128 + 64) * 0x10000
    b = (t * 3 `mod` 256     ) * 0x100
    a = 0xFF

prettyMandelbrot :: Int -> StepPlane U -> MBitmap F -> IO ()
prettyMandelbrot limit zs mbmap =
    loadP (map (prettyRGBA limit) zs) mbmap

type MandelFun =  R
               -> R
               -> R
               -> R
               -> Int
               -> Int
               -> Int
               -> IO (Bitmap F)

data MandelState = MandelState
    { manDim   :: DIM2
    , manMbmap :: MBitmap F
    }

mandelbrotImageGenerator :: IO MandelFun
mandelbrotImageGenerator = do
    mbmap    <- newMVec 0
    stateRef <- newIORef (MandelState (ix2 0 0) mbmap)
    return $ mandelbrotImage stateRef
  where
    mandelbrotImage :: IORef MandelState -> MandelFun
    mandelbrotImage stateRef lowx lowy highx highy viewx viewy depth = do
        let sh =  ix2 viewy viewx
        state  <- updateState stateRef sh
        zs     <- mandelbrot lowx lowy highx highy viewx viewy depth
        prettyMandelbrot depth zs (manMbmap state)
        unsafeFreezeMVec sh (manMbmap state)

    updateState :: IORef MandelState -> DIM2 -> IO MandelState
    updateState stateRef sh = do
        state <- readIORef stateRef
        if manDim state == sh
          then return state
          else newState
      where
        newState :: IO MandelState
        newState = do
            mbmap      <- newMVec (size sh)
            let state' =  MandelState sh mbmap
            writeIORef stateRef state'
            return state'

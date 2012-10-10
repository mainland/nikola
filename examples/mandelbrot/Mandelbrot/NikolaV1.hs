{-# LANGUAGe BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGe TemplateHaskell #-}

module Mandelbrot.NikolaV1 (mandelbrotImage) where

import Data.Array.Nikola.Backend.CUDA.TH (compile)
import Data.Array.Repa
import Data.Array.Repa.Repr.ForeignPtr
import Data.Array.Repa.Repr.UnboxedForeign
import Data.Array.Repa.Repr.CUDA.UnboxedForeign
import qualified Data.Vector.UnboxedForeign as VUF
import qualified Data.Vector.Storable as V
import Data.Int
import Data.Word

import qualified Mandelbrot.NikolaV1.Implementation as I

type R = Double

type Complex = (R, R)

type RGBA = Word32

type Bitmap r = Array r DIM2 RGBA

type ComplexPlane r = Array r DIM2 Complex

type StepPlane r = Array r DIM2 (Complex, Int32)

step :: ComplexPlane CUF -> StepPlane CUF -> IO (StepPlane CUF)
step = $(compile I.step)

genPlane :: R
         -> R
         -> R
         -> R
         -> Int32
         -> Int32
         -> IO (ComplexPlane CUF)
genPlane = $(compile I.genPlane)

mkinit :: ComplexPlane CUF -> IO (StepPlane CUF)
mkinit = $(compile I.mkinit)

prettyMandelbrot :: Int32 -> StepPlane CUF -> IO (Bitmap F)
prettyMandelbrot limit arr = do
    bmap                              <- prettyMandelbrotDev limit arr
    let AFUnboxed sh (VUF.V_Word32 v) =  toHostArray bmap
    let (fp, n)                       =  V.unsafeToForeignPtr0 v
    return $ AForeignPtr sh n fp
  where
    prettyMandelbrotDev :: Int32 -> StepPlane CUF -> IO (Bitmap CUF)
    prettyMandelbrotDev = $(compile I.prettyMandelbrot)

mandelbrot :: R
           -> R
           -> R
           -> R
           -> Int32
           -> Int32
           -> Int32
           -> IO (StepPlane CUF)
mandelbrot lowx lowy highx highy viewx viewy depth = do
    cs  <- genPlane lowx lowy highx highy viewx viewy
    zs0 <- mkinit cs
    iterateM (step cs) depth zs0

iterateM :: Monad m => (a -> m a) -> Int32 -> a -> m a
iterateM f = go
  where
    go 0 x = return x
    go n x = f x >>= go (n-1)

mandelbrotImage :: R
                -> R
                -> R
                -> R
                -> Int
                -> Int
                -> Int
                -> IO (Bitmap F)
mandelbrotImage lowx lowy highx highy viewx viewy depth =
    mandelbrot lowx' lowy' highx' highy' viewx' viewy' depth' >>=
    prettyMandelbrot depth'
  where
    lowx', lowy', highx', highy' :: R
    lowx'  = realToFrac lowx
    lowy'  = realToFrac lowy
    highx' = realToFrac highx
    highy' = realToFrac highy
    viewx', viewy', depth' :: Int32
    viewx' = fromIntegral viewx
    viewy' = fromIntegral viewy
    depth' = fromIntegral depth

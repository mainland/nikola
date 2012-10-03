{-# LANGUAGe BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGe TemplateHaskell #-}

module Mandelbrot.NikolaV1
    ( mandelbrot
    , prettyMandelbrot
    ) where

import Data.Array.Nikola.Backend.CUDA.TH
import Data.Array.Repa
import Data.Array.Repa.Repr.ForeignPtr
import Data.Array.Repa.Repr.UnboxedForeign
import Data.Array.Repa.Repr.CUDA.UnboxedForeign
import qualified Data.Vector.UnboxedForeign as VUF
import qualified Data.Vector.Storable as V

import qualified Mandelbrot.NikolaV1.Implementation as I
import Mandelbrot.Types

step :: ComplexPlane CUF -> StepPlane CUF -> IO (StepPlane CUF)
step = $(compile I.step)

genPlane :: R
         -> R
         -> R
         -> R
         -> I
         -> I
         -> IO (ComplexPlane CUF)
genPlane = $(compile I.genPlane)

mkinit :: ComplexPlane CUF -> IO (StepPlane CUF)
mkinit = $(compile I.mkinit)

prettyMandelbrot :: I -> StepPlane CUF -> IO (Bitmap F)
prettyMandelbrot limit arr = do
    bmap                              <- prettyMandelbrotDev limit arr
    let AFUnboxed sh (VUF.V_Word32 v) =  toHostArray bmap
    let (fp, n)                       =  V.unsafeToForeignPtr0 v
    return $ AForeignPtr sh n fp
  where
    prettyMandelbrotDev :: I -> StepPlane CUF -> IO (Bitmap CUF)
    prettyMandelbrotDev = $(compile I.prettyMandelbrot)

mandelbrot :: R
           -> R
           -> R
           -> R
           -> I
           -> I
           -> I
           -> IO (StepPlane CUF)
mandelbrot lowx lowy highx highy viewx viewy depth = do
    cs  <- genPlane lowx lowy highx highy viewx viewy
    zs0 <- mkinit cs
    iterateM (step cs) depth zs0

iterateM :: Monad m => (a -> m a) -> I -> a -> m a
iterateM f = go
  where
    go 0 x = return x
    go n x = f x >>= go (n-1)

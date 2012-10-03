{-# LANGUAGe BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGe TemplateHaskell #-}

module Mandelbrot.NikolaV2 (mandelbrotImage) where

import Control.Monad (replicateM_)
import Data.Int
import Data.Word

import Data.Array.Nikola.Backend.CUDA.TH
import Data.Array.Repa
import Data.Array.Repa.Mutable
import Data.Array.Repa.Repr.ForeignPtr
import Data.Array.Repa.Repr.UnboxedForeign
import Data.Array.Repa.Repr.CUDA.UnboxedForeign
import qualified Data.Vector.UnboxedForeign as VUF
import qualified Data.Vector.Storable as V

import qualified Mandelbrot.NikolaV2.Implementation as I

type R = Double

type RGBA = Word32

type Bitmap r = Array r DIM2 RGBA

type Complex = (R, R)

type ComplexPlane r = Array r DIM2 Complex

type MComplexPlane r = MArray r DIM2 Complex

type StepPlane r = Array r DIM2 (Complex, Int32)

type MStepPlane r = MArray r DIM2 (Complex, Int32)

step :: ComplexPlane CUF -> MStepPlane CUF -> IO ()
step = $(compile I.step)

genPlane :: R
         -> R
         -> R
         -> R
         -> Int32
         -> Int32
         -> MComplexPlane CUF
         -> IO ()
genPlane = $(compile I.genPlane)

mkinit :: ComplexPlane CUF -> MStepPlane CUF -> IO ()
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
    mcs <- newMArray sh
    genPlane lowx lowy highx highy viewx viewy mcs
    cs  <- unsafeFreezeMArray mcs
    mzs <- newMArray sh
    mkinit cs mzs
    replicateM_ (fromIntegral depth) (step cs mzs)
    unsafeFreezeMArray mzs
  where
    sh :: DIM2
    sh = ix2 (fromIntegral viewy) (fromIntegral viewx)

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

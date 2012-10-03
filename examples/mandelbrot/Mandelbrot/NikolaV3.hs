{-# LANGUAGe BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGe TemplateHaskell #-}

module Mandelbrot.NikolaV3
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

import qualified Mandelbrot.NikolaV3.Implementation as I
import Mandelbrot.Types

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
mandelbrot = $(compile I.mandelbrot)

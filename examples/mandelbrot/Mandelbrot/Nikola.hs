{-# LANGUAGe BangPatterns #-}
{-# LANGUAGe TemplateHaskell #-}

module Mandelbrot.Nikola
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

import qualified Mandelbrot.Nikola.Implementation as I
import Mandelbrot.Types

step :: ComplexPlane CUF -> StepPlane CUF -> StepPlane CUF
step = $(compile I.step)

genPlane :: R
         -> R
         -> R
         -> R
         -> I
         -> I
         -> ComplexPlane CUF
genPlane = $(compile I.genPlane)

mkinit :: ComplexPlane CUF -> StepPlane CUF
mkinit = $(compile I.mkinit)

prettyMandelbrot :: I -> StepPlane CUF -> Bitmap F
prettyMandelbrot limit arr =
    let AFUnboxed sh (VUF.V_Word32 v) = toHostArray $ prettyMandelbrotDev limit arr
        (fp, n)                       = V.unsafeToForeignPtr0 v
    in
      AForeignPtr sh n fp

prettyMandelbrotDev :: I -> StepPlane CUF -> Bitmap CUF
prettyMandelbrotDev = $(compile I.prettyMandelbrot)

mandelbrot :: R
           -> R
           -> R
           -> R
           -> I
           -> I
           -> I
           -> StepPlane CUF
mandelbrot lowx lowy highx highy viewx viewy depth =
    iter (step cs) (fromIntegral depth) zs0
  where
    cs :: ComplexPlane CUF
    cs = genPlane lowx lowy highx highy viewx viewy

    zs0 :: StepPlane CUF
    zs0 = mkinit cs

-- We need to use 'iter' instead of a combination of 'iterate' and '(!!)'
-- because 'iterate' builds up a list of results, none of which can be
-- GC'd. Since the results are in GPU memory, that causes us to quickly run out
-- of memory. We also need the 'seq' in 'iter' to avoid building up a big thunk
-- which results in the same problem of retaining too much GPU memory!
iter :: (a -> a) -> Int -> a -> a
iter f = go
  where
    go 0 !x = x `seq` x
    go n !x = x `seq` go (n-1) (f x)

{-# LANGUAGe BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGe TemplateHaskell #-}

module Mandelbrot.NikolaV4 (mandelbrotImageGenerator) where

import Data.IORef
import Data.Int
import Data.Word

import Data.Array.Nikola.Backend.CUDA.TH
import Data.Array.Repa
import Data.Array.Repa.Eval
import Data.Array.Repa.Mutable
import Data.Array.Repa.Repr.ForeignPtr          as F
import Data.Array.Repa.Repr.UnboxedForeign      as UF
import Data.Array.Repa.Repr.CUDA.UnboxedForeign as CUF
import qualified Data.Vector.UnboxedForeign as VUF
import qualified Data.Vector.Storable as V

import qualified Mandelbrot.NikolaV4.Implementation as I

type R = Double

type Complex = (R, R)

type RGBA = Word32

type Bitmap r = Array r DIM2 RGBA

type MBitmap r = MArray r DIM2 RGBA

type ComplexPlane r = Array r DIM2 Complex

type MComplexPlane r = MArray r DIM2 Complex

type StepPlane r = Array r DIM2 (Complex, Int32)

type MStepPlane r = MArray r DIM2 (Complex, Int32)

prettyMandelbrot :: Int32 -> StepPlane CUF -> MBitmap CUF -> IO ()
prettyMandelbrot = $(compile I.prettyMandelbrot)

mandelbrot :: R
           -> R
           -> R
           -> R
           -> Int32
           -> Int32
           -> Int32
           -> MComplexPlane CUF
           -> MStepPlane CUF
           -> IO ()
mandelbrot = $(compile I.mandelbrot)

type MandelFun =  R
               -> R
               -> R
               -> R
               -> Int
               -> Int
               -> Int
               -> IO (Bitmap F)

data MandelState = MandelState
    { manDim    :: DIM2
    , manMBmapH :: MVec UF RGBA
    , manMBmapD :: MBitmap CUF
    , manMCs    :: MComplexPlane CUF
    , manMZs    :: MStepPlane CUF
    }

mandelbrotImageGenerator :: IO MandelFun
mandelbrotImageGenerator = do
    state    <- newState (ix2 0 0)
    stateRef <- newIORef state
    return $ mandelbrotImage stateRef
  where
    mandelbrotImage :: IORef MandelState -> MandelFun
    mandelbrotImage stateRef lowx lowy highx highy viewx viewy depth = do
        let sh     =  ix2 viewy viewx
        state      <- updateState stateRef sh
        let mbmapH =  manMBmapH state
            mbmapD =  manMBmapD state
            mcs    =  manMCs state
            mzs    =  manMZs state
        mandelbrot lowx' lowy' highx' highy' viewx' viewy' depth' mcs mzs
        zs     <- unsafeFreezeMArray mzs
        prettyMandelbrot depth' zs mbmapD
        bmapD  <- unsafeFreezeMArray mbmapD
        loadHostP bmapD mbmapH
        bmapH  <- unsafeFreezeMVec sh mbmapH
        return (convertBitmap bmapH)
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

    convertBitmap :: Bitmap UF -> Bitmap F
    convertBitmap bmap =
        let VUF.V_Word32 v = UF.toUnboxedForeign bmap
            (fp, _)        = V.unsafeToForeignPtr0 v
        in
          F.fromForeignPtr (extent bmap) fp

    newState :: DIM2 -> IO MandelState
    newState sh = do
        mbmapH <- newMVec (size sh)
        mbmapD <- newMArray sh
        mcs    <- newMArray sh
        mzs    <- newMArray sh
        return $ MandelState sh mbmapH mbmapD mcs mzs

    updateState :: IORef MandelState -> DIM2 -> IO MandelState
    updateState stateRef sh = do
        state <- readIORef stateRef
        if manDim state == sh
          then return state
          else do state' <- newState sh
                  writeIORef stateRef state'
                  return state'

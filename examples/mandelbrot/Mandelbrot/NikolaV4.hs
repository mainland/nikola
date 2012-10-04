{-# LANGUAGe BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGe TemplateHaskell #-}

module Mandelbrot.NikolaV4 (mandelbrotImageGenerator) where

import Control.Exception
import Data.IORef
import Data.Int
import Data.Word
import Foreign (sizeOf, nullPtr)

import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))

import Foreign.CUDA.ForeignPtr (newForeignDevPtr_)
import qualified Foreign.CUDA.Driver.Graphics        as CUG
import qualified Foreign.CUDA.Driver.Graphics.OpenGL as CUGL

import Data.Array.Nikola.Backend.CUDA.TH
import Data.Array.Repa
import Data.Array.Repa.Mutable
import Data.Array.Repa.Repr.CUDA.UnboxedForeign    as CUF
import qualified Data.Vector.CUDA.Storable.Mutable as MVCS
import qualified Data.Vector.CUDA.UnboxedForeign   as VCUF

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
               -> IO GL.BufferObject

data MandelState = MandelState
    { manDim    :: DIM2
    , manMCs    :: MComplexPlane CUF
    , manMZs    :: MStepPlane CUF
    }

mandelbrotImageGenerator :: IO MandelFun
mandelbrotImageGenerator = do
    state    <- newState (ix2 0 0)
    stateRef <- newIORef state
    [pbo]    <- GL.genObjectNames 1
    return $ mandelbrotImage stateRef pbo
  where
    mandelbrotImage :: IORef MandelState
                    -> GL.BufferObject
                    -> MandelFun
    mandelbrotImage stateRef pbo lowx lowy highx highy viewx viewy depth = do
        let sh     =  ix2 viewy viewx
        state      <- updateState stateRef pbo sh
        let mcs    =  manMCs state
            mzs    =  manMZs state
        mandelbrot lowx' lowy' highx' highy' viewx' viewy' depth' mcs mzs
        zs <- unsafeFreezeMArray mzs

        withPBOResource pbo $ \pboRes ->
            withMappedResource pboRes $
            withResourceMBitmap sh pboRes $ \mbmapD ->
            prettyMandelbrot depth' zs mbmapD

        return pbo
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

    newState :: DIM2 -> IO MandelState
    newState sh = do
        mcs    <- newMArray sh
        mzs    <- newMArray sh
        return $ MandelState sh mcs mzs

    updateState :: IORef MandelState -> GL.BufferObject -> DIM2 -> IO MandelState
    updateState stateRef pbo sh = do
        state <- readIORef stateRef
        if manDim state == sh
          then return state
          else do resizePBO pbo sh
                  state' <- newState sh
                  writeIORef stateRef state'
                  return state'

resizePBO :: GL.BufferObject -> DIM2 -> IO ()
resizePBO pbo sh = do
    let nBytes = size sh*sizeOf (undefined :: RGBA)
    GL.bindBuffer GL.PixelUnpackBuffer $= Just pbo
    GL.bufferData GL.PixelUnpackBuffer $= (fromIntegral nBytes, nullPtr, GL.DynamicCopy)
    GL.bindBuffer GL.PixelUnpackBuffer $= Nothing

withPBOResource :: GL.BufferObject -> (CUG.Resource -> IO a) -> IO a
withPBOResource pbo =
    bracket (CUGL.registerBuffer pbo CUG.RegisterNone)
            CUG.unregisterResource

withMappedResource :: CUG.Resource -> IO a -> IO a
withMappedResource res =
    bracket_ (CUG.mapResources [res] Nothing)
             (CUG.unmapResources [res] Nothing)

withResourceMBitmap :: DIM2 -> CUG.Resource -> (MBitmap CUF -> IO a) -> IO a
withResourceMBitmap sh res kont = do
    (dptr, _)  <- CUG.getMappedPointer res
    fdptr      <- newForeignDevPtr_ dptr
    let mv     =  MVCS.unsafeFromForeignDevPtr0 fdptr (size sh)
    kont $ CUF.fromMUnboxedForeign sh (VCUF.MV_Word32 mv)

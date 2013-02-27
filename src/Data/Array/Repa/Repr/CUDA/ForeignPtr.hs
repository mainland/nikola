{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : Data.Array.Repa.Repr.CUDA.ForeignPtr
-- Copyright   : (c) Geoffrey Mainland 2012
-- License     : BSD-style
--
-- Maintainer  : Geoffrey Mainland <mainland@apeiron.net>
-- Stability   : experimental
-- Portability : non-portable

module Data.Array.Repa.Repr.CUDA.ForeignPtr (
    CF,
    Array(..),
    MArray(..),

    fromForeignDevPtr, toForeignDevPtr,
    fromMForeignDevPtr, toMForeignDevPtr
  ) where

import Control.Monad
import Data.Array.Repa
import Data.Array.Repa.Eval
import Data.Array.Repa.Mutable
import Foreign (Storable,
                peek,
                poke,
                alloca)
import qualified Foreign.CUDA.Driver     as CU
import qualified Foreign.CUDA.ForeignPtr as CU
import System.IO.Unsafe

-- | Arrays represented as CUDA device pointers.
data CF

instance Storable a => Source CF a where
    data Array CF sh a = AFDP !sh !Int !(CU.ForeignDevicePtr a)

    {-# INLINE linearIndex #-}
    linearIndex (AFDP _ len fdptr) ix
        | ix < len  = unsafePerformIO $
                      alloca $ \ptr -> do
                      CU.withForeignDevPtr fdptr $ \dptr -> do
                        CU.peekArray 1 (dptr `CU.advanceDevPtr` ix) ptr
                      peek ptr

        | otherwise = error "Repa: device array index out of bounds"

    {-# INLINE unsafeLinearIndex #-}
    unsafeLinearIndex (AFDP _ _ fdptr) ix =
        unsafePerformIO $
        alloca $ \ptr -> do
        CU.withForeignDevPtr fdptr $ \dptr -> do
          CU.peekArray 1 (dptr `CU.advanceDevPtr` ix) ptr
        peek ptr

    {-# INLINE extent #-}
    extent (AFDP sh _ _) = sh

    {-# INLINE deepSeqArray #-}
    deepSeqArray (AFDP sh len fdptr) x =
        sh `deepSeq` len `seq` fdptr `seq` x

-- | Filling device buffers.
instance Storable e => Target CF e where
    data MVec CF e = FDPVec !Int !(CU.ForeignDevicePtr e)

    {-# INLINE newMVec #-}
    newMVec n = do
        fdptr <- CU.mallocForeignDevPtrArray n
        return $ FDPVec n fdptr

    {-# INLINE unsafeWriteMVec #-}
    unsafeWriteMVec (FDPVec _ fdptr) !ix !x =
        alloca $ \ptr -> do
        poke ptr x
        CU.withForeignDevPtr fdptr $ \dptr -> do
        CU.pokeArray 1 ptr (dptr `CU.advanceDevPtr` ix)

    {-# INLINE unsafeFreezeMVec #-}
    unsafeFreezeMVec !sh (FDPVec len fdptr) =
        return $ AFDP sh len fdptr

    {-# INLINE deepSeqMVec #-}
    deepSeqMVec !(FDPVec _ fdptr) x =
        fdptr `seq` x

    {-# INLINE touchMVec #-}
    touchMVec (FDPVec _ fdptr) =
        CU.touchForeignDevPtr fdptr

-- | Mutable unboxed vector arrays.
instance (Storable e, Shape sh) => Mutable CF sh e where
    data MArray CF sh e = AMFDP !sh !Int !(CU.ForeignDevicePtr e)

    {-# INLINE mextent #-}
    mextent (AMFDP sh _ _) = sh

    {-# INLINE newMArray #-}
    newMArray sh = liftM (AMFDP sh (size sh)) (CU.mallocForeignDevPtrArray (size sh))

    {-# INLINE unsafeWriteMArray #-}
    unsafeWriteMArray (AMFDP sh _ fdptr) ix x =
        alloca $ \ptr -> do
        poke ptr x
        CU.withForeignDevPtr fdptr $ \dptr -> do
        CU.pokeArray (toIndex sh ix) ptr dptr

    {-# INLINE unsafeFreezeMArray #-}
    unsafeFreezeMArray (AMFDP sh sz fdptr) =
        return $ AFDP sh sz fdptr

-- | O(1). Wrap a 'ForeignDevicePtr' as an array.
fromForeignDevPtr :: Shape sh
                  => sh -> CU.ForeignDevicePtr e -> Array CF sh e
{-# INLINE fromForeignDevPtr #-}
fromForeignDevPtr !sh !fdptr = AFDP sh (size sh) fdptr

-- | O(1). Unpack a 'ForeignDevicePtr' from an array.
toForeignDevPtr :: Array CF sh e -> CU.ForeignDevicePtr e
{-# INLINE toForeignDevPtr #-}
toForeignDevPtr (AFDP _ _ fdptr) = fdptr

-- | O(1). Wrap a 'ForeignDevicePtr' as an array.
fromMForeignDevPtr :: Shape sh
                   => sh -> CU.ForeignDevicePtr e -> MArray CF sh e
{-# INLINE fromMForeignDevPtr #-}
fromMForeignDevPtr !sh !fdptr = AMFDP sh (size sh) fdptr

-- | O(1). Unpack a 'ForeignDevicePtr' from an array.
toMForeignDevPtr :: MArray CF sh e -> CU.ForeignDevicePtr e
{-# INLINE toMForeignDevPtr #-}
toMForeignDevPtr (AMFDP _ _ fdptr) = fdptr

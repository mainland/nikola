{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Array.Repa.Repr.CUDA (
    CU,
    Array (..),
    fromDevicePtr,
    toDevicePtr
  ) where

import Data.Array.Repa
import Data.Array.Repa.Eval
import Foreign.Storable
import Foreign.Marshal.Alloc
import System.IO.Unsafe
import Foreign.CUDA.Driver

-- | Arrays represented as CUDA device pointers.
data CU

instance Storable a => Source CU a where
    data Array CU sh a = ADevicePtr !sh !Int !(DevicePtr a)

    {-# INLINE linearIndex #-}
    linearIndex (ADevicePtr _ len devptr) ix
        | ix < len  = unsafePerformIO $
                      alloca $ \ptr -> do
                      peekArray 1 (devptr `advanceDevPtr` ix) ptr
                      peek ptr

        | otherwise = error "Repa: device array index out of bounds"

    {-# INLINE unsafeLinearIndex #-}
    unsafeLinearIndex (ADevicePtr _ _ devptr) ix =
        unsafePerformIO $
        alloca $ \ptr -> do
        peekArray 1 (devptr `advanceDevPtr` ix) ptr
        peek ptr

    {-# INLINE extent #-}
    extent (ADevicePtr sh _ _) = sh

    {-# INLINE deepSeqArray #-}
    deepSeqArray (ADevicePtr sh len devptr) x =
        sh `deepSeq` len `seq` devptr `seq` x

-- | Filling device buffers.
instance Storable e => Target CU e where
    data MVec CU e = CUPVec !Int !(DevicePtr e)

    {-# INLINE newMVec #-}
    newMVec n = do
        devptr <- mallocArray n
        return $ CUPVec n devptr

    {-# INLINE unsafeWriteMVec #-}
    unsafeWriteMVec (CUPVec _ devptr) !ix !x =
        alloca $ \ptr -> do
        poke ptr x
        pokeArray 1 ptr (devptr `advanceDevPtr` ix)

    {-# INLINE unsafeFreezeMVec #-}
    unsafeFreezeMVec !sh (CUPVec len devptr) =
        return $ ADevicePtr sh len devptr

    {-# INLINE deepSeqMVec #-}
    deepSeqMVec !(CUPVec _ devptr) x =
        devPtrToWordPtr devptr `seq` x

    {-# INLINE touchMVec #-}
    touchMVec (CUPVec _ _) =
        return ()

-- | O(1). Wrap a `DevicePtr` as an array.
fromDevicePtr :: Shape sh
               => sh -> DevicePtr e -> Array CU sh e
{-# INLINE fromDevicePtr #-}
fromDevicePtr !sh !devptr = ADevicePtr sh (size sh) devptr

-- | O(1). Unpack a `DevicePtr` from an array.
toDevicePtr :: Array CU sh e -> DevicePtr e
{-# INLINE toDevicePtr #-}
toDevicePtr (ADevicePtr _ _ devptr) = devptr

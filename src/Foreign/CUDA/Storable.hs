-- |
-- Module      : Foreign.CUDA.Storable
-- Copyright   : (c) Geoffrey Mainland 2012
-- License     : BSD-style
--
-- Maintainer  : Geoffrey Mainland <mainland@apeiron.net>
-- Stability   : experimental
-- Portability : non-portable

module Foreign.CUDA.Storable
    ( peekDevElemOff
    , pokeDevElemOff
    , peekDevByteOff
    , pokeDevByteOff
    , peekDev
    , pokeDev
    ) where

import Foreign.CUDA.Driver.Marshal
import Foreign.CUDA.Ptr
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable

peekDevElemOff :: Storable a => DevicePtr a -> Int -> IO a
{-# INLINE peekDevElemOff #-}
peekDevElemOff dp i =
    alloca $ \p -> do
    peekArray 1 (advanceDevPtr dp i) p
    peek p

pokeDevElemOff :: Storable a => DevicePtr a -> Int -> a -> IO ()
{-# INLINE pokeDevElemOff #-}
pokeDevElemOff dp i x =
    alloca $ \p -> do
    poke p x
    pokeArray 1 p (advanceDevPtr dp i)

peekDevByteOff :: Storable a => DevicePtr b -> Int -> IO a
{-# INLINE peekDevByteOff #-}
peekDevByteOff ptr off = peekDev (ptr `plusDevPtr'` off)

pokeDevByteOff :: Storable a => DevicePtr b -> Int -> a -> IO ()
{-# INLINE pokeDevByteOff #-}
pokeDevByteOff ptr off = pokeDev (ptr `plusDevPtr'` off)

peekDev :: Storable a => DevicePtr a -> IO a
{-# INLINE peekDev #-}
peekDev ptr = peekDevElemOff ptr 0

pokeDev :: Storable a => DevicePtr a -> a -> IO ()
{-# INLINE pokeDev #-}
pokeDev ptr = pokeDevElemOff ptr 0

plusDevPtr' :: DevicePtr a -> Int -> DevicePtr b
plusDevPtr' (DevicePtr p) d = DevicePtr (p `plusPtr` d)

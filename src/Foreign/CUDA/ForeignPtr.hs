{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Foreign.CUDA.ForeignPtr
-- Copyright   : (c) Geoffrey Mainland 2012
-- License     : BSD-style
--
-- Maintainer  : Geoffrey Mainland <mainland@apeiron.net>
-- Stability   : experimental
-- Portability : non-portable

module Foreign.CUDA.ForeignPtr
    ( ForeignDevicePtr
    , castForeignDevPtr
    , newForeignDevPtr
    , newForeignDevPtr_
    , mallocDeviceArray
    , mallocForeignDevPtrArray
    , withForeignDevPtr
    , unsafeForeignDevPtrToDevPtr
    , touchForeignDevPtr
    ) where

import Prelude hiding (catch)

import Control.Concurrent (yield)
import Foreign.CUDA.Driver
import Foreign.Concurrent (newForeignPtr)
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Storable
import GHC.ForeignPtr (ForeignPtr(..))
import GHC.Ptr (Ptr(..))
import System.Mem (performGC)

type ForeignDevicePtr a = ForeignPtr a

castForeignDevPtr :: ForeignDevicePtr a -> ForeignDevicePtr b
castForeignDevPtr = castForeignPtr

newForeignDevPtr :: DevicePtr a -> IO (ForeignDevicePtr a)
newForeignDevPtr dptr = newForeignPtr (useDevicePtr dptr) (free dptr)

newForeignDevPtr_ :: DevicePtr a -> IO (ForeignDevicePtr a)
newForeignDevPtr_ dptr = newForeignPtr_ (useDevicePtr dptr)

-- | Allocate an array on a CUDA device. We used to call 'performGC' when
-- allocation failed and then try once more, but that doesn't work! We need the
-- GC to finalize unreachable 'ForeignDevicePtr's, but I don't know how to get
-- that to happen reliably. The solution below seems to work well, but it does
-- mean every GPU allocation requires invoking the GC.
mallocDeviceArray :: Storable a => Int -> IO (DevicePtr a)
mallocDeviceArray n = do
    performGC
    yield
    mallocArray n'
  where
    n' :: Int
    n' = max 1 n

mallocForeignDevPtrArray :: Storable a => Int -> IO (ForeignDevicePtr a)
mallocForeignDevPtrArray n = do
    dptr <- mallocDeviceArray n
    newForeignDevPtr dptr

withForeignDevPtr :: ForeignDevicePtr a -> (DevicePtr a -> IO b) -> IO b
withForeignDevPtr fdptr f =
    withForeignPtr fdptr $ \ptr -> f (DevicePtr ptr)

unsafeForeignDevPtrToDevPtr :: ForeignDevicePtr a -> DevicePtr a
unsafeForeignDevPtrToDevPtr (ForeignPtr addr _) = DevicePtr (Ptr addr)

touchForeignDevPtr :: ForeignDevicePtr a -> IO ()
touchForeignDevPtr = touchForeignPtr

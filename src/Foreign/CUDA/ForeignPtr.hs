{-# LANGUAGE ForeignFunctionInterface #-}
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
import Control.Exception
import Foreign.CUDA.Driver
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable
import GHC.ForeignPtr (ForeignPtr(..))
import GHC.Ptr (Ptr(..))
import System.Mem (performGC)

type ForeignDevicePtr a = ForeignPtr a

castForeignDevPtr :: ForeignDevicePtr a -> ForeignDevicePtr b
castForeignDevPtr = castForeignPtr

newForeignDevPtr :: DevicePtr a -> IO (ForeignDevicePtr a)
newForeignDevPtr dptr = newForeignPtr cuMemFree_ (useDevicePtr dptr)

foreign import ccall unsafe "&cuMemFree_"
  cuMemFree_ :: FunPtr (Ptr a -> IO ())

newForeignDevPtr_ :: DevicePtr a -> IO (ForeignDevicePtr a)
newForeignDevPtr_ dptr = newForeignPtr_ (useDevicePtr dptr)

-- | Allocate an array on a CUDA device. When allocation fails, we call
-- performGC, yield, and try again. Thanks to Simon Marlow for recommending the
-- use of a C finalizer---Haskell finalizers don't get called predictably enough
-- to make ForeignDevicePtr's useful.
mallocDeviceArray :: Storable a => Int -> IO (DevicePtr a)
mallocDeviceArray n =
    mallocArray n' `catch` \(_ :: CUDAException) ->
        performGC >> yield >> mallocArray n'
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

-- |
-- Module      : Data.Vector.CUDA.Storable.Internal
-- Copyright   : (c) Roman Leshchinskiy 2009-2010
-- Copyright   : (c) Geoffrey Mainland 2012
-- License     : BSD-style
--
-- Maintainer  : Geoffrey Mainland <mainland@apeiron.net>
-- Stability   : experimental
-- Portability : non-portable
--
-- Ugly internal utility functions for implementing 'Storable'-based vectors.
--

module Data.Vector.CUDA.Storable.Internal (
  getPtr, setPtr, updPtr
) where

import Foreign.CUDA.ForeignPtr
import Foreign.CUDA.Ptr
import GHC.ForeignPtr   ( ForeignPtr(..) )
import GHC.Ptr          ( Ptr(..) )

getPtr :: ForeignDevicePtr a -> DevicePtr a
{-# INLINE getPtr #-}
getPtr (ForeignPtr addr _) =
    DevicePtr (Ptr addr)

setPtr :: ForeignDevicePtr a -> DevicePtr a -> ForeignDevicePtr a
{-# INLINE setPtr #-}
setPtr (ForeignPtr _ c) (DevicePtr (Ptr addr)) =
    ForeignPtr addr c

updPtr :: (DevicePtr a -> DevicePtr a) -> ForeignDevicePtr a -> ForeignDevicePtr a
{-# INLINE updPtr #-}
updPtr f (ForeignPtr p c) =
    case f (DevicePtr (Ptr p)) of
      DevicePtr (Ptr q) -> ForeignPtr q c

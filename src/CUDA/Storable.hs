-- Copyright (c) 2009-2010
--         The President and Fellows of Harvard College.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions
-- are met:
-- 1. Redistributions of source code must retain the above copyright
--    notice, this list of conditions and the following disclaimer.
-- 2. Redistributions in binary form must reproduce the above copyright
--    notice, this list of conditions and the following disclaimer in the
--    documentation and/or other materials provided with the distribution.
-- 3. Neither the name of the University nor the names of its contributors
--    may be used to endorse or promote products derived from this software
--    without specific prior written permission.

-- THIS SOFTWARE IS PROVIDED BY THE UNIVERSITY AND CONTRIBUTORS ``AS IS'' AND
-- ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
-- IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
-- ARE DISCLAIMED.  IN NO EVENT SHALL THE UNIVERSITY OR CONTRIBUTORS BE LIABLE
-- FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
-- OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
-- HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
-- LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
-- OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
-- SUCH DAMAGE.

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module CUDA.Storable (
    Storable(..)
  ) where

import Control.Monad.State
import Foreign hiding (Storable(..))
import qualified Foreign
import Foreign.C.Types

import CUDA.Internal

align :: Storable a => Int -> a -> Int
align off x = (off + alignment x - 1) .&. complement (alignment x - 1)

-- | A type that can be stored in GPU memory (passed to/returned from a GPU
-- kernel).
class Storable a where
    -- |Size of @a@ on the GPU.
    sizeOf :: a -> Int

    -- |Alignment of @a@ on the GPU.
    alignment :: a -> Int

    -- |Set a parameter that will be passed to the kernel next time it is
    -- invoked.
    setParam :: CUFunction -- ^The kernel
             -> Int        -- ^A byte offset
             -> a          -- ^The parameter
             -> IO Int     -- ^Byte offset updated to account for the space used
                           -- by the parameter.

instance Storable Bool where
    sizeOf _    = sizeOf (undefined :: CUChar)
    alignment _ = sizeOf (undefined :: CUChar)

    setParam f off False = setParam f off (0 :: CUChar)
    setParam f off True  = setParam f off (1 :: CUChar)

instance Storable Int where
    sizeOf _    = sizeOf (undefined :: CLLong)
    alignment _ = sizeOf (undefined :: CLLong)

    setParam f off x =
        setParam f off (fromIntegral x :: CLLong)

instance Storable Float where
    sizeOf _    = sizeOf (undefined :: CFloat)
    alignment _ = sizeOf (undefined :: CFloat)

    setParam f off x = do
        cuParamSetf f off' (realToFrac x)
        return (off' + sizeOf x)
      where
        off' = align off x

instance Storable CUChar where
    sizeOf _    = Foreign.sizeOf (undefined :: CUChar)
    alignment _ = Foreign.sizeOf (undefined :: CUChar)

    setParam f off x =
        allocaBytes (Foreign.sizeOf x) $ \ptr -> do
        Foreign.poke ptr x
        cuParamSetv f off' ptr (Foreign.sizeOf x)
        return (off' + sizeOf x)
      where
        off' = align off x

instance Storable CInt where
    sizeOf _    = Foreign.sizeOf (undefined :: CInt)
    alignment _ = Foreign.sizeOf (undefined :: CInt)

    setParam f off x = do
        cuParamSeti f off' (fromIntegral x)
        return (off' + sizeOf x)
      where
        off' = align off x

instance Storable CLLong where
    sizeOf _    = Foreign.sizeOf (undefined :: CLLong)
    alignment _ = Foreign.sizeOf (undefined :: CLLong)

    setParam f off x =
        allocaBytes (Foreign.sizeOf x) $ \ptr -> do
        Foreign.poke ptr x
        cuParamSetv f off' ptr (Foreign.sizeOf x)
        return (off' + sizeOf x)
      where
        off' = align off x

instance Storable CFloat where
    sizeOf _    = Foreign.sizeOf (undefined :: CFloat)
    alignment _ = Foreign.sizeOf (undefined :: CFloat)

    setParam f off x =
        cuParamSetf f off' (realToFrac x) >> return (off' + sizeOf x)
      where
        off' = align off x

instance Storable a => Storable (CUDevicePtr a) where
    sizeOf _    = 8
    alignment _ = 8

    setParam f off devPtr =
        withDevicePtrAddress devPtr $ \ptr -> do
        cuParamSetv f off' ptr (sizeOf devPtr)
        return (off' + sizeOf devPtr)
      where
        off' = align off devPtr

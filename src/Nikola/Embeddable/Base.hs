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

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Nikola.Embeddable.Base (
    Embeddable
  ) where

import CUDA.Internal
import CUDA.Storable
import Control.Monad (liftM)
import Control.Monad.Trans (liftIO)
import Foreign hiding (Storable(..))
import Foreign (peek)
import qualified Foreign
import Foreign.C.Types

import Nikola.Syntax
import Nikola.Exec
import Nikola.Embeddable.Class

instance Embeddable () where
    type Rep () = CLLong

    embeddedType _ _ = ScalarT UnitT

    withArg _ act = do
        pushArg (ScalarArg)
        act

    returnResult =
        return ()

instance IsScalar () where
    toRep = undefined
    fromRep = undefined

    embeddedBaseType _ = UnitT

instance Embeddable Int where
    type Rep Int = CLLong

    embeddedType _ _ = ScalarT IntT

    withArg n act = do
        pushArg (ScalarArg)
        pushParam n
        act

    returnResult = do
        devPtr :: CUDevicePtr CLLong <- liftM (castCUDevicePtr . allocPtr) $ popAlloc
        x <- liftIO $ allocaBytes count $ \hostPtr -> do
             cuMemcpyDtoH devPtr hostPtr count
             peek hostPtr
        liftIO $ cuMemFree devPtr
        return (fromIntegral x)
      where
        count = sizeOf (undefined :: CLLong)

instance IsScalar Int where
    toRep = fromIntegral
    fromRep = fromIntegral

    embeddedBaseType _ = IntT

instance Embeddable Float where
    type Rep Float = CFloat

    embeddedType _ _ = ScalarT FloatT

    withArg n act = do
        pushArg ScalarArg
        pushParam n
        act

    returnResult = do
        devPtr :: CUDevicePtr CFloat <- liftM (castCUDevicePtr . allocPtr) $ popAlloc
        x <- liftIO $ allocaBytes count $ \hostPtr -> do
             cuMemcpyDtoH devPtr hostPtr count
             peek hostPtr
        liftIO $ cuMemFree devPtr
        return (realToFrac x)
      where
        count = sizeOf (undefined :: Float)

instance IsScalar Float where
    toRep = realToFrac
    fromRep = realToFrac

    embeddedBaseType _ = FloatT

instance (IsScalar a, Storable a, Foreign.Storable (Rep a))
    => Embeddable [a] where
    type Rep [a] = CUDevicePtr (Rep a)

    embeddedType _ n =
        vectorT tau n
      where
        tau = embeddedBaseType (undefined :: a)

    withArg xs act = do
        CUVector n devPtr <- liftIO $ toCUVector xs

        pushArg (VectorArg n)
        pushParam devPtr
        pushParam (fromIntegral n :: CInt)

        result <- act
        liftIO $ cuMemFree devPtr
        return result

    returnResult = do
        count :: Int         <- returnResult
        VectorAlloc devPtr _ <- popAlloc
        xs <- liftIO $ fromCUVector (CUVector count (castCUDevicePtr devPtr))
        liftIO $ cuMemFree devPtr
        return xs

instance (IsScalar a, Storable a, Foreign.Storable (Rep a))
    => IsVector [] a where
    fromList = id
    toList = id

    fromCUVector (CUVector 0 _) =
        return []

    fromCUVector (CUVector count devPtr) = do
        xs <- allocaBytes byteCount $ \(ptr :: Ptr (Rep a)) -> do
              cuMemcpyDtoH (castCUDevicePtr devPtr) ptr byteCount
              peekArray count ptr
        return (map fromRep xs)
      where
        byteCount = count * sizeOf (undefined :: Rep a)

    toCUVector xs = do
        devPtr <- liftIO $ cuMemAlloc byteCount
        allocaBytes byteCount $ \(ptr :: Ptr (Rep a)) -> do
            pokeArray ptr (map toRep xs)
            cuMemcpyHtoD ptr devPtr byteCount
        return (CUVector count devPtr)
      where
        count :: Int
        count = length xs

        byteCount :: Int
        byteCount = count * sizeOf (undefined :: Rep a)


instance (IsScalar a, Storable a, Foreign.Storable (Rep a))
    => Embeddable (CUVector a) where
    type Rep (CUVector a) = CUDevicePtr (Rep a)

    embeddedType _ n =
        vectorT tau n
      where
        tau = embeddedBaseType (undefined :: a)

    withArg (CUVector n devPtr) act = do
        pushArg (VectorArg n)
        pushParam devPtr
        pushParam (fromIntegral n :: CInt)

        act

    returnResult = do
        count :: Int         <- returnResult
        VectorAlloc devPtr _ <- popAlloc
        return $ CUVector count (castCUDevicePtr devPtr)

instance (IsScalar a, Storable a, Foreign.Storable (Rep a))
    => IsVector CUVector a where
    fromList = error ""
    toList = error ""

    fromCUVector = fail ""
    toCUVector = fail ""

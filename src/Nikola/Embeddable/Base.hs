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

import Control.Monad (liftM)
import Control.Monad.Trans (liftIO)
import qualified Foreign.CUDA.Driver as CU
import Foreign
import Foreign.C.Types

import Nikola.Syntax
import Nikola.Exec
import Nikola.Embeddable.Class

instance Embeddable () where
    type Rep () = CInt

    embeddedType _ _ = ScalarT UnitT

    withArg _ act = do
        pushArg (IntArg 0)
        act

    returnResult =
        return ()

instance IsScalar () where
    toRep = undefined
    fromRep = undefined

    embeddedBaseType _ = UnitT

instance Embeddable Int where
    type Rep Int = CInt

    embeddedType _ _ = ScalarT IntT

    withArg n act = do
        pushArg (IntArg n)
        act

    returnResult = do
        devPtr :: CU.DevicePtr (Rep Int) <- liftM (CU.castDevPtr . allocPtr) $ popAlloc
        x <- liftIO $ alloca $ \hostPtr -> do
             CU.peekArray 1 devPtr hostPtr
             peek hostPtr
        liftIO $ CU.free devPtr
        return (fromIntegral x)

instance IsScalar Int where
    toRep = fromIntegral
    fromRep = fromIntegral

    embeddedBaseType _ = IntT

instance Embeddable Float where
    type Rep Float = CFloat

    embeddedType _ _ = ScalarT FloatT

    withArg n act = do
        pushArg (FloatArg n)
        act

    returnResult = do
        devPtr :: CU.DevicePtr Float <- liftM (CU.castDevPtr . allocPtr) $ popAlloc
        x <- liftIO $ alloca $ \hostPtr -> do
             CU.peekArray 1 devPtr hostPtr
             peek hostPtr
        liftIO $ CU.free devPtr
        return (realToFrac x)

instance IsScalar Float where
    toRep = realToFrac
    fromRep = realToFrac

    embeddedBaseType _ = FloatT

instance (IsScalar a, Storable a, Foreign.Storable (Rep a))
    => Embeddable [a] where
    type Rep [a] = CU.DevicePtr (Rep a)

    embeddedType _ n =
        vectorT tau n
      where
        tau = embeddedBaseType (undefined :: a)

    withArg xs act = do
        CUVector n devPtr <- liftIO $ toCUVector xs
        pushArg (VectorArg n (CU.castDevPtr devPtr))

        result <- act
        liftIO $ CU.free devPtr
        return result

    returnResult = do
        count :: Int          <- returnResult
        VectorAlloc _ devPtr  <- popAlloc
        xs <- liftIO $ fromCUVector (CUVector count (CU.castDevPtr devPtr))
        liftIO $ CU.free devPtr
        return xs

instance (IsScalar a, Storable a, Foreign.Storable (Rep a))
    => IsVector [] a where
    fromList = id
    toList = id

    fromCUVector (CUVector 0 _) =
        return []

    fromCUVector (CUVector count devPtr) = do
        xs <- allocaArray count $ \(ptr :: Ptr (Rep a)) -> do
              CU.peekArray count (CU.castDevPtr devPtr) ptr
              peekArray count ptr
        return (map fromRep xs)

    toCUVector xs = do
        devPtr <- liftIO $ CU.mallocArray count
        allocaArray count $ \(ptr :: Ptr (Rep a)) -> do
            pokeArray ptr (map toRep xs)
            CU.pokeArray count ptr devPtr
        return (CUVector count devPtr)
      where
        count :: Int
        count = length xs

instance (IsScalar a, Storable a, Foreign.Storable (Rep a))
    => Embeddable (CUVector a) where
    type Rep (CUVector a) = CU.DevicePtr (Rep a)

    embeddedType _ n =
        vectorT tau n
      where
        tau = embeddedBaseType (undefined :: a)

    withArg (CUVector n devPtr) act = do
        pushArg (VectorArg n (CU.castDevPtr devPtr))
        act

    returnResult = do
        count :: Int         <- returnResult
        VectorAlloc _ devPtr <- popAlloc
        return $ CUVector count (CU.castDevPtr devPtr)

instance (IsScalar a, Storable a, Foreign.Storable (Rep a))
    => IsVector CUVector a where
    fromList = error ""
    toList = error ""

    fromCUVector = fail ""
    toCUVector = fail ""

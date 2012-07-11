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
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Nikola.Embeddable.Vector (
    Embeddable,
    IsVector
  ) where

import Control.Monad.Trans (liftIO)
import qualified Data.Vector.Storable as V
import Foreign
import qualified Foreign.CUDA.Driver as CU

import Nikola.Embeddable.Base ()
import Nikola.Embeddable.Class
import Nikola.Exec
import Nikola.Syntax

instance (IsScalar a, Storable a, Storable (Rep a))
    => Embeddable (V.Vector a) where
    type Rep (V.Vector a) = CU.DevicePtr (Rep a)

    embeddedType _ n =
        vectorT tau n
      where
        tau = embeddedBaseType (undefined :: a)

    withArg v act = do
        CUVector n devPtr <- liftIO $ toCUVector v

        pushArg (VectorArg n (CU.castDevPtr devPtr))

        result <- act
        liftIO $ CU.free devPtr
        return result

    returnResult = do
        count :: Int         <- returnResult
        VectorAlloc _ devPtr <- popAlloc
        xs <- liftIO $ fromCUVector (CUVector count (CU.castDevPtr devPtr))
        liftIO $ CU.free devPtr
        return xs

instance (IsScalar a, Storable a, Storable (Rep a))
    => IsVector V.Vector a where
    fromList = V.fromList
    toList = V.toList

    fromCUVector (CUVector 0 _) =
        return V.empty

    fromCUVector (CUVector count devPtr) = do
        fptr <- mallocForeignPtrArray count
        withForeignPtr fptr $ \ptr ->
            CU.peekArray count (CU.castDevPtr devPtr) ptr
        return $ V.unsafeFromForeignPtr fptr 0 count

    toCUVector v = do
        devPtr <- liftIO $ CU.mallocArray n
        liftIO $ V.unsafeWith v $ \ptr ->
                 CU.pokeArray n ptr devPtr
        return (CUVector n (CU.castDevPtr devPtr))
      where
        n :: Int
        n = V.length v

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

module Nikola.Embeddable.Class (
    CUVector(..),
    unsafeWithNewVector,
    unsafeFreeVector,

    Embeddable(..),
    IsScalar(..),
    IsVector(..),
    IsMatrix(..)
  ) where

import CUDA.Internal
import CUDA.Storable
import Control.Applicative
import Control.Exception
import Data.Typeable

import Nikola.Exec
import Nikola.Syntax

data CUVector a = CUVector !Int !(CUDevicePtr (Rep a))
  deriving (Typeable)

unsafeWithNewVector :: forall a b . Storable (Rep a)
                    => Int
                    -> (CUVector a -> IO b)
                    -> IO b
unsafeWithNewVector n =
    bracket alloc free
  where
    alloc = CUVector n <$> cuMemAlloc bytes
    bytes = n * sizeOf (undefined :: Rep a)

    free (CUVector _ ptr) = cuMemFree ptr

unsafeFreeVector :: CUVector a -> IO ()
unsafeFreeVector (CUVector _ devPtr) =
    cuMemFree devPtr

-- | A type that can be used in a GPU embedding.
class (Typeable a, Storable (Rep a)) => Embeddable a where
    -- | The /representation/ type for @a@ when a value of type @a@ is
    -- tranferred to the GPU.
    type Rep a :: *

    -- | The embedded type that corresponds to 'a'.
    embeddedType :: a -> Int -> Rho

    -- | Extend the current execution context with an argument of type 'a' and
    -- then continue by performing an 'Ex' action..
    withArg :: a -> Ex b -> Ex b

    -- | An 'Ex' action that returns a result of type 'a'.
    returnResult :: Ex a

-- | A scalar type that can be used in GPU code. This is used to constrain the
-- set of legal types that can appear in containers, e.g., vectors and matrices.
class Embeddable a => IsScalar a where
    -- | Convert to representation type.
    toRep :: a -> Rep a

    -- | Convert from representation type.
    fromRep :: Rep a -> a

    embeddedBaseType :: a -> Tau

-- | A vector type that can be used in GPU code
class Embeddable a => IsVector f a where
    fromList :: [a] -> f a
    toList :: f a -> [a]

    -- |Create an @f a@ of the specified length from a device pointer.
    fromCUVector :: CUVector a -> IO (f a)

    -- |Create a device pointer from a @f a@.
    toCUVector :: f a -> IO (CUVector a)

-- | A matrix type that can be used in GPU code
class Embeddable a => IsMatrix f a where
    fromLists :: [[a]] -> f a
    toLists :: f a -> [[a]]

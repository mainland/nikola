-- Copyright (c) 2009-2012
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

{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Nikola.Embeddable (
    CUVector(..),
    unsafeWithNewVector,
    unsafeFreeVector,

    Embeddable(..),
    IsScalar(..),
    IsVector(..),
    IsMatrix(..)
  ) where

import Control.Applicative
import Control.Monad (liftM)
import Control.Monad.Trans (liftIO)
import Control.Exception
#if defined(HMATRIX)
import Data.Packed.Development
import Data.Packed.Matrix
#endif /* defined(HMATRIX) */
import Data.Typeable
import qualified Data.Vector.Storable as V
import qualified Foreign.CUDA.Driver as CU
import Foreign
import Foreign.C.Types

import Nikola.Exec
import Nikola.Syntax

data CUVector a = CUVector !Int !(CU.DevicePtr (Rep a))
  deriving (Typeable)

unsafeWithNewVector :: forall a b . Storable (Rep a)
                    => Int
                    -> (CUVector a -> IO b)
                    -> IO b
unsafeWithNewVector n =
    bracket alloc free
  where
    alloc = CUVector n <$> CU.mallocArray n

    free (CUVector _ ptr) = CU.free ptr

unsafeFreeVector :: CUVector a -> IO ()
unsafeFreeVector (CUVector _ devPtr) =
    CU.free devPtr

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

#if defined(HMATRIX)
deriving instance Typeable1 Matrix

-- | A helper function that adapts 'Data.Packed.Development.mat' to a more
-- traditional \"with\"-style interface. @f@ is passed the number of rows, the
-- number of columns, and a 'Ptr' to the matrix contents.
withMatrix  ::  Storable t
            =>  Matrix t
            ->  (CInt -> CInt -> Ptr t -> IO ())
            ->  IO ()
withMatrix m f = mat m $ \g -> g f

instance (Element a, IsScalar a, Storable a)
    => Embeddable (Matrix a) where
    type Rep (Matrix a) = CU.DevicePtr (Rep a)

    embeddedType _ n =
        matrixT tau n n n
      where
        tau = embeddedBaseType (undefined :: a)

    withArg m act = do
        devPtr <- liftIO $ CU.mallocArray n
        liftIO $ withMatrix (fmat m) $ \_ _ ptr ->
                 CU.pokeArray n ptr devPtr

        pushArg (MatrixArg r r c (CU.castDevPtr devPtr))

        result <- act
        liftIO $ CU.free devPtr
        return result
      where
        r, c, n :: Int
        r = rows m
        c = cols m
        n = r * c

    returnResult = do
        MatrixAlloc _ r c devPtr <- popAlloc
        r      <-  evalN r
        c      <-  evalN c
        let n  =   r * c
        liftIO $ do  m <- createMatrix ColumnMajor r c
                     withMatrix m $ \_ _ ptr ->
                         CU.peekArray n (CU.castDevPtr devPtr) ptr
                     CU.free devPtr
                     return m
#endif /* defined(HMATRIX) */

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
--
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
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Array.Nikola.Backend.CUDA.Target (
    CUDA,

    fromCUDARep,
    toCUDARep,
    embeddedCUDAType,
    withCUDAArg,
    returnCUDAResult,

    Vector(..),
    unsafeWithNewVector,
    unsafeFreeVector
  ) where

import Control.Applicative
import Control.Exception
import Control.Monad.Trans (liftIO)
import Data.Int
#if defined(HMATRIX)
import Data.Packed.Development
import Data.Packed.Matrix
#endif /* defined(HMATRIX) */
import Data.Typeable
import qualified Data.Vector.Storable as V
import qualified Foreign.CUDA.Driver as CU
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable

import Data.Array.Nikola.Backend.CUDA.Exec
import Data.Array.Nikola.Embed
import Data.Array.Nikola.Language.Syntax

-- | Vectors whose contents exist in GPU memory
data Vector a = Vector {-# UNPACK #-} !Int
                       {-# UNPACK #-} !(CU.DevicePtr (Rep a))
  deriving (Eq, Ord, Typeable)

instance Show (Vector a) where
  showsPrec n (Vector _ p) = showsPrec n p

unsafeWithNewVector :: Storable (Rep a)
                    => Int
                    -> (Vector a -> IO b)
                    -> IO b
unsafeWithNewVector n =
    bracket alloc free
  where
    alloc = Vector n <$> CU.mallocArray n

    free (Vector _ ptr) = CU.free ptr

unsafeFreeVector :: Vector a -> IO ()
unsafeFreeVector (Vector _ devPtr) =
    CU.free devPtr

-- | The CUDA target
data CUDA
  deriving (Typeable)

fromCUDARep :: Representable CUDA a => Rep a -> IO a
fromCUDARep = fromRep (undefined :: CUDA)

toCUDARep :: Representable CUDA a => a -> IO (Rep a)
toCUDARep = toRep (undefined :: CUDA)

embeddedCUDAType :: Representable CUDA a => a -> ParamIdx -> Tau
embeddedCUDAType = embeddedType (undefined :: CUDA)

withCUDAArg :: Representable CUDA a => a -> CallCtx CUDA b -> CallCtx CUDA b
withCUDAArg = withArg (undefined :: CUDA)

returnCUDAResult :: Representable CUDA a => CallCtx CUDA a
returnCUDAResult = returnResult (undefined :: CUDA)

instance Representable CUDA () where
    type CallCtx CUDA = Ex

    type Rep () = CInt

    toRep _ () = return  0

    fromRep _ _ = return ()

    embeddedType _ _ _ = UnitT

    withArg _ _ act = do
        pushArg (IntArg 0)
        act

    returnResult _ =
        return ()

instance Elt CUDA () where

instance Representable CUDA Int32 where
    type CallCtx CUDA = Ex

    type Rep Int32 = Int32

    toRep _ = return . fromIntegral

    fromRep _ = return . fromIntegral

    embeddedType _ _ _ = Int32T

    withArg _ n act = do
        pushArg (IntArg (fromIntegral n))
        act

    returnResult _ = do
        devPtr :: CU.DevicePtr (Rep Int32) <- (CU.castDevPtr . allocPtr) <$> popAlloc
        x <- liftIO $ alloca $ \hostPtr -> do
             CU.peekArray 1 devPtr hostPtr
             peek hostPtr
        liftIO $ CU.free devPtr
        return (fromIntegral x)

instance Elt CUDA Int32 where

instance Representable CUDA Float where
    type CallCtx CUDA = Ex

    type Rep Float = CFloat

    toRep _ = return . realToFrac

    fromRep _ = return . realToFrac

    embeddedType _ _ _ = FloatT

    withArg _ n act = do
        pushArg (FloatArg n)
        act

    returnResult _ = do
        devPtr :: CU.DevicePtr Float <- (CU.castDevPtr . allocPtr) <$> popAlloc
        x <- liftIO $ alloca $ \hostPtr -> do
             CU.peekArray 1 devPtr hostPtr
             peek hostPtr
        liftIO $ CU.free devPtr
        return (realToFrac x)

instance Elt CUDA Float where

instance (Elt CUDA a, Storable a, Storable (Rep a))
    => Representable CUDA [a] where
    type CallCtx CUDA = Ex

    type Rep [a] = Vector a

    toRep _ xs = do
        devPtr <- liftIO $ CU.mallocArray n
        allocaArray n $ \(ptr :: Ptr (Rep a)) -> do
            mapM toCUDARep xs >>= pokeArray ptr
            CU.pokeArray n ptr devPtr
        return (Vector n devPtr)
      where
        n :: Int
        n = length xs

    fromRep _ (Vector n devPtr) = do
        xs <- allocaArray n $ \(ptr :: Ptr (Rep a)) -> do
              CU.peekArray n (CU.castDevPtr devPtr) ptr
              peekArray n ptr
        mapM fromCUDARep xs

    embeddedType _ _ n =
        vectorArgT (embeddedCUDAType (undefined :: a) n) n

    withArg _ xs act = do
        Vector n devPtr <- liftIO $ toCUDARep xs
        pushArg (ArrayArg [n] [] (CU.castDevPtr devPtr))

        result <- act
        liftIO $ CU.free devPtr
        return result

    returnResult _ = do
        count :: Int32         <- returnCUDAResult
        ArrayAlloc _ _ devPtr  <- popAlloc
        xs <- liftIO $ fromCUDARep (Vector (fromIntegral count)
                                           (CU.castDevPtr devPtr))
        liftIO $ CU.free devPtr
        return xs

instance (Elt CUDA a, Storable a, Storable (Rep a))
    => IsVector CUDA [] a where

instance (Elt CUDA a, Storable a)
    => Representable CUDA (Vector a) where
    type CallCtx CUDA = Ex

    type Rep (Vector a) = Vector a

    fromRep _ = return

    toRep _ = return

    embeddedType _ _ n =
        vectorArgT (embeddedCUDAType (undefined :: a) n) n

    withArg _ (Vector n devPtr) act = do
        pushArg (ArrayArg [n] [] (CU.castDevPtr devPtr))
        act

    returnResult _ = do
        count :: Int32        <- returnCUDAResult
        ArrayAlloc _ _ devPtr <- popAlloc
        return $ Vector (fromIntegral count) (CU.castDevPtr devPtr)

instance (Elt CUDA a, Storable a)
    => IsVector CUDA Vector a where

instance (Elt CUDA a, Storable a)
    => Representable CUDA (V.Vector a) where
    type CallCtx CUDA = Ex

    type Rep (V.Vector a) = Vector a

    toRep _ v = do
        devPtr <- liftIO $ CU.mallocArray n
        liftIO $ V.unsafeWith v $ \ptr ->
                 CU.pokeArray n ptr devPtr
        return (Vector n (CU.castDevPtr devPtr))
      where
        n :: Int
        n = V.length v

    fromRep _ (Vector n devPtr) = do
        fptr <- mallocForeignPtrArray n
        withForeignPtr fptr $ \ptr ->
            CU.peekArray n (CU.castDevPtr devPtr) ptr
        return $ V.unsafeFromForeignPtr fptr 0 n

    embeddedType _ _ n =
        vectorArgT (embeddedCUDAType (undefined :: a) n) n

    withArg _ v act = do
        Vector n devPtr <- liftIO $ toCUDARep v

        pushArg (ArrayArg [n] [] (CU.castDevPtr devPtr))

        result <- act
        liftIO $ CU.free devPtr
        return result

    returnResult _ = do
        count :: Int32        <- returnCUDAResult
        ArrayAlloc _ _ devPtr <- popAlloc
        xs <- liftIO $ fromCUDARep (Vector (fromIntegral count)
                                           (CU.castDevPtr devPtr))
        liftIO $ CU.free devPtr
        return xs

instance (Elt CUDA a, Storable a)
    => IsVector CUDA V.Vector a where

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

instance (Element a, Elt CUDA a, Storable a)
    => Representable CUDA (Matrix a) where
    type CallCtx CUDA = Ex

    type Rep (Matrix a) = CU.DevicePtr (Rep a)

    toRep _ _ = fail "toRep undefined for Matrix"

    fromRep _ _ = fail "fromRep undefined for Matrix"

    embeddedType _ _ n =
        matrixArgT (embeddedCUDAType (undefined :: a) n) n

    withArg _ m act = do
        devPtr <- liftIO $ CU.mallocArray n
        liftIO $ withMatrix (fmat m) $ \_ _ ptr ->
                 CU.pokeArray n ptr devPtr

        pushArg (ArrayArg [r, c] [r] (CU.castDevPtr devPtr))

        result <- act
        liftIO $ CU.free devPtr
        return result
      where
        r, c, n :: Int
        r = rows m
        c = cols m
        n = r * c

    returnResult _ = do
        ArrayAlloc [r, c] [_] devPtr <- popAlloc
        r      <-  evalN r
        c      <-  evalN c
        let n  =   r * c
        liftIO $ do  m <- createMatrix ColumnMajor r c
                     withMatrix m $ \_ _ ptr ->
                         CU.peekArray n (CU.castDevPtr devPtr) ptr
                     CU.free devPtr
                     return m
#endif /* defined(HMATRIX) */

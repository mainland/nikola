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

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Nikola.Embeddable.Hmatrix (
    Embeddable
  ) where

import CUDA.Internal
import CUDA.Storable
import Control.Monad.Trans (liftIO)
import Data.Packed.Development
import Data.Packed.Matrix
import Data.Typeable
import Foreign hiding (Storable(..))
import qualified Foreign
import Foreign.C.Types

import Nikola.Embeddable.Base ()
import Nikola.Embeddable.Class
import Nikola.Exec
import Nikola.Syntax

deriving instance Typeable1 Matrix

-- | A helper function that adapts 'Data.Packed.Development.mat' to a more
-- traditional \"with\"-style interface. @f@ is passed the number of rows, the
-- number of columns, and a 'Ptr' to the matrix contents.
withMatrix  ::  Foreign.Storable t
            =>  Matrix t
            ->  (CInt -> CInt -> Ptr t -> IO ())
            ->  IO ()
withMatrix m f = mat m $ \g -> g f

instance (Element a, IsScalar a, Storable a, Foreign.Storable a)
    => Embeddable (Matrix a) where
    type Rep (Matrix a) = CUDevicePtr (Rep a)

    embeddedType _ n =
        matrixT tau n n n
      where
        tau = embeddedBaseType (undefined :: a)

    withArg m act = do
        devPtr <- liftIO $ cuMemAlloc byteCount
        liftIO $ withMatrix (fmat m) $ \_ _ ptr ->
                 cuMemcpyHtoD ptr devPtr byteCount

        pushArg (MatrixArg r r c)
        pushParam devPtr
        pushParam (fromIntegral r :: CInt)
        pushParam (fromIntegral r :: CInt)
        pushParam (fromIntegral c :: CInt)

        result <- act
        liftIO $ cuMemFree devPtr
        return result
      where
        r, c :: Int
        r = rows m
        c = cols m

        byteCount :: Int
        byteCount = r * c * sizeOf (undefined :: a)

    returnResult = do
        MatrixAlloc devPtr _ r c <- popAlloc
        r <- evalN r
        c <- evalN c
        let byteCount = r * c * sizeOf (undefined :: a)
        liftIO $ do  m <- createMatrix ColumnMajor r c
                     withMatrix m $ \_ _ ptr ->
                         cuMemcpyDtoH (castCUDevicePtr devPtr) ptr byteCount
                     cuMemFree devPtr
                     return m

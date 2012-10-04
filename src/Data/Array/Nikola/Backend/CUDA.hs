{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- |
-- Module      : Data.Array.Nikola.Backend.CUDA
-- Copyright   : (c) Geoffrey Mainland 2012
-- License     : BSD-style
--
-- Maintainer  : Geoffrey Mainland <mainland@apeiron.net>
-- Stability   : experimental
-- Portability : non-portable

module Data.Array.Nikola.Backend.CUDA (
    module Data.Array.Nikola.Backend.C.Codegen,

    module Data.Array.Nikola.Array,
    module Data.Array.Nikola.Exp,
    module Data.Array.Nikola.Operators.IndexSpace,
    module Data.Array.Nikola.Operators.Mapping,
    -- module Data.Array.Nikola.Operators.Reduction,
    module Data.Array.Nikola.Program,
    module Data.Array.Nikola.Repr.Delayed,
    module Data.Array.Nikola.Repr.Global,
    module Data.Array.Nikola.Repr.Push,

    initializeCUDACtx,

    CUGL.DeviceList(..),
    initializeCUDAGLCtx,

    CUDA,
    Vector,

    sizeOfT,

    Exp,

    Sh.Shape(..),
    Sh.Z(..),
    (Sh.:.)(..),
    Sh.Rsh,

    DIM0,
    DIM1,
    DIM2,

    Sh.ix1,
    Sh.ix2,

    defaultMain,

    vapply
  ) where

import qualified Prelude as P
import Prelude hiding (catch)

import Control.Exception
import Data.Int
import Data.Typeable (Typeable)
import Data.Word
import qualified Foreign.CUDA.Driver as CU
import qualified Foreign.CUDA.Driver.Graphics.OpenGL as CUGL
import Foreign.Storable (sizeOf)

import Data.Array.Nikola.Backend.Main
import Data.Array.Nikola.Backend.C.Codegen

import Data.Array.Nikola.Array
import qualified Data.Array.Nikola.Exp as E
import Data.Array.Nikola.Exp hiding (Exp)
import Data.Array.Nikola.Operators.IndexSpace
import Data.Array.Nikola.Operators.Mapping
--import Data.Array.Nikola.Operators.Reduction
import Data.Array.Nikola.Program
import Data.Array.Nikola.Repr.Delayed
import Data.Array.Nikola.Repr.Global
import Data.Array.Nikola.Repr.Push
import qualified Data.Array.Nikola.Shape as Sh

import Data.Array.Nikola.Language.Reify
import Data.Array.Nikola.Language.Syntax hiding (Exp)

initializeCUDACtx :: IO CU.Context
initializeCUDACtx = do
    CU.initialise []
    ndevs <- CU.count
    ctxCreate 0 ndevs
  where
    ctxCreate :: Int -> Int -> IO CU.Context
    ctxCreate i n | i >= n = CU.cudaError "Can't create a context"
    ctxCreate i n =
        (CU.device i >>= \dev -> CU.create dev [])
      `catch` \(_ :: CU.CUDAException) -> ctxCreate (i+1) n

initializeCUDAGLCtx :: CUGL.DeviceList -> IO CU.Context
initializeCUDAGLCtx deviceList = do
    CU.initialise []
    devs <- CUGL.getGLDevices 255 deviceList
    case devs of
      dev:_ -> CUGL.createGLContext dev []
      _     -> CU.cudaError "No device available for OpenGl interoperability"

-- | The CUDA target
data CUDA
  deriving (Typeable)

#define cudaIsElem(ty,tycon)                                       \
instance IsElem (E.Exp CUDA ty) where                              \
{ type Rep (E.Exp CUDA ty) = ty                                    \
; typeOf _ = tycon                                                 \
; indexElem arr ix = E $ IndexE (unE arr) (unE ix)                 \
; writeElem arr ix x = seqK (WriteE (unE arr) (unE ix) (unE x)) () \
}

cudaIsElem(Int8,   Int8T)
cudaIsElem(Int16,  Int16T)
cudaIsElem(Int32,  Int32T)
cudaIsElem(Int64,  Int64T)
cudaIsElem(Word8,  Word8T)
cudaIsElem(Word16, Word16T)
cudaIsElem(Word32, Word32T)
cudaIsElem(Word64, Word64T)
cudaIsElem(Float,  FloatT)
cudaIsElem(Double, DoubleT)

instance IsNum CUDA Int8 where
instance IsNum CUDA Int16 where
instance IsNum CUDA Int32 where
instance IsNum CUDA Int64 where
instance IsNum CUDA Word8 where
instance IsNum CUDA Word16 where
instance IsNum CUDA Word32 where
instance IsNum CUDA Word64 where
instance IsNum CUDA Float where
instance IsNum CUDA Double where

type Vector r a = Array r DIM1 a

type Exp a = E.Exp CUDA a

sizeOfT :: ScalarType -> Int
sizeOfT UnitT   = 0
sizeOfT BoolT   = sizeOf (undefined :: Word8)
sizeOfT Int8T   = sizeOf (undefined :: Int8)
sizeOfT Int16T  = sizeOf (undefined :: Int16)
sizeOfT Int32T  = sizeOf (undefined :: Int32)
sizeOfT Int64T  = sizeOf (undefined :: Int64)
sizeOfT Word8T  = sizeOf (undefined :: Word8)
sizeOfT Word16T = sizeOf (undefined :: Word16)
sizeOfT Word32T = sizeOf (undefined :: Word32)
sizeOfT Word64T = sizeOf (undefined :: Word64)
sizeOfT FloatT  = sizeOf (undefined :: Float)
sizeOfT DoubleT = sizeOf (undefined :: Double)
sizeOfT tau     = error ("Cannot determine size of type " P.++ show tau)

-- Common dimensions
type DIM0 = Sh.DIM0 CUDA
type DIM1 = Sh.DIM1 CUDA
type DIM2 = Sh.DIM2 CUDA

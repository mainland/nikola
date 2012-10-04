{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : Data.Array.Nikola.Repr.Global
-- Copyright   : (c) Geoffrey Mainland 2012
-- License     : BSD-style
--
-- Maintainer  : Geoffrey Mainland <mainland@apeiron.net>
-- Stability   : experimental
-- Portability : non-portable

module Data.Array.Nikola.Repr.Global (
    G,
    Array(..),
    MArray(..),

    IsElem(..)
  ) where

import Data.Typeable (Typeable)
--import Text.PrettyPrint.Mainland

import Data.Array.Nikola.Array
import Data.Array.Nikola.Exp
import Data.Array.Nikola.Shape
import Data.Array.Nikola.Eval.Target

import Data.Array.Nikola.Language.Check
import Data.Array.Nikola.Language.Monad
import qualified Data.Array.Nikola.Language.Syntax as S
import Data.Array.Nikola.Language.Syntax hiding (Exp, Var)

-- | "Global" arrays exist in GPU global memory.
data G
  deriving (Typeable)

instance IsElem a => IsArray G a where
    data Array G sh a = AGlobal sh S.Exp

    extent (AGlobal sh _) = sh

instance IsElem e => Source G e where
    index (AGlobal sh arr) ix =
        indexElem (E arr) (toIndex sh ix)

    linearIndex (AGlobal _ arr) ix =
        indexElem (E arr) ix

instance IsElem e => Target G e where
    data MArray G sh e = MGlobal sh S.Exp

    mextent (MGlobal sh _) = sh

    newMArray sh = do
        v         <- gensym "vec_alloca"
        let atau  =  ArrayT tau (rank sh)
        shiftH $ \k -> do
            p         <- reset $ extendVarTypes [(v, atau)] $ k ()
            let alloc =  AllocE atau (map unE (listOfShape sh))
            return $ BindE v atau alloc p
        shift $ \k -> do
        extendVarTypes [(v, atau)] $ k (MGlobal sh (VarE v))
      where
        tau :: ScalarType
        tau = typeOf (undefined :: e)

    unsafeWriteMArray (MGlobal sh arr) idx x =
        writeElem (E arr) (toIndex sh idx) x

    unsafeFreezeMArray (MGlobal sh arr) =
        return $ AGlobal sh arr

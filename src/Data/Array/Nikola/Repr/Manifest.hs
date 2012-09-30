{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : Data.Array.Nikola.Repr.Manifest
-- Copyright   : (c) Geoffrey Mainland 2012
-- License     : BSD-style
--
-- Maintainer  : Geoffrey Mainland <mainland@apeiron.net>
-- Stability   : experimental
-- Portability : non-portable

module Data.Array.Nikola.Repr.Manifest (
    M,
    Array(..),

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

-- | "Manifest" arrays represented by an array expression
data M
  deriving (Typeable)

instance IsElem a => IsArray M a where
    data Array M sh a = AManifest sh S.Exp

    extent (AManifest sh _) = sh

instance IsElem e => Source M e where
    index (AManifest sh arr) ix =
        indexElem (E arr) (toIndex sh ix)

    linearIndex (AManifest _ arr) ix =
        indexElem (E arr) ix

instance IsElem e => Target M e where
    data MArray M sh e = MManifest sh S.Exp

    newMArray sh = do
        v         <- gensym "vec_alloca"
        let atau  =  ArrayT tau (rank sh)
        shiftH $ \k -> do
            p         <- reset $ extendVarTypes [(v, atau)] $ k ()
            let alloc =  AllocE atau (map unE (listOfShape sh))
            return $ BindE v atau alloc p
        shift $ \k -> do
        extendVarTypes [(v, atau)] $ k (MManifest sh (VarE v))
      where
        tau :: ScalarType
        tau = typeOf (undefined :: e)

    unsafeWriteMArray (MManifest sh arr) idx x =
        writeElem (E arr) (toIndex sh idx) x

    unsafeFreezeMArray (MManifest sh arr) =
        return $ AManifest sh arr

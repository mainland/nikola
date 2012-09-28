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

    IsElem(..),

    Manifest(..),

    alloca,
    write,
    mkManifest
  ) where

import Data.Typeable (Typeable)
--import Text.PrettyPrint.Mainland

import Data.Array.Nikola.Array
import Data.Array.Nikola.Exp
import Data.Array.Nikola.Shape

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

instance IsElem a => Source M a where
    index (AManifest sh arr) ix =
        indexElem (E arr) (toIndex sh ix)

    linearIndex (AManifest _ arr) ix =
        indexElem (E arr) ix

class (IsElem a, IsArray r a) => Manifest r a where
    manifest :: (Shape sh)
             => Array r sh a
             -> Array M sh a
             -> P ()

instance IsElem a => Manifest M a where
    manifest v1@(AManifest sh _) v2 = do
        i <- parfor sh
        write v2 i (index v1 i)

alloca :: forall sh a . (IsElem a, Shape sh)
       => sh
       -> P (Array M sh a)
alloca sh = do
    v         <- gensym "vec_alloca"
    let atau  =  ArrayT tau (rank sh)
    shiftH $ \k -> do
        p         <- reset $ extendVarTypes [(v, atau)] $ k ()
        let alloc =  AllocE atau (map unE (listOfShape sh))
        return $ BindE v atau alloc p
    shift $ \k -> do
    extendVarTypes [(v, atau)] $ k (AManifest sh (VarE v))
  where
    tau :: ScalarType
    tau = typeOf (undefined :: a)

write :: forall sh a . (IsElem a, Shape sh)
      => Array M sh a
      -> sh
      -> a
      -> P ()
write (AManifest sh arr) idx x =
    writeElem (E arr) (toIndex sh idx) x

mkManifest :: (Shape sh, IsElem a, Manifest r a)
           => Array r sh a
           -> P (Array M sh a)
mkManifest arr = do
    arr' <- alloca (extent arr)
    manifest arr arr'
    return arr'

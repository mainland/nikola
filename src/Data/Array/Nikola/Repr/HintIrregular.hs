{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : Data.Array.Nikola.Repr.HintIrregular
-- Copyright   : (c) Geoffrey Mainland 2012
-- License     : BSD-style
--
-- Maintainer  : Geoffrey Mainland <mainland@apeiron.net>
-- Stability   : experimental
-- Portability : non-portable

module Data.Array.Nikola.Repr.HintIrregular (
    I,
    Array(..),
    MArray(..),

    hintIrregular
  ) where

import Control.Applicative
import Data.Traversable
import Data.Typeable (Typeable)

import Data.Array.Nikola.Array
import Data.Array.Nikola.Eval.Target
import Data.Array.Nikola.Eval.Load

import Data.Array.Nikola.Language.Check
import Data.Array.Nikola.Language.Generic
import Data.Array.Nikola.Language.Monad
import Data.Array.Nikola.Language.Syntax hiding (Exp, Var)

-- | Hints that the work done computing this array is irregular.
data I r
  deriving (Typeable)

instance IsArray r e => IsArray (I r) e where
    data Array (I r) sh e = AIrregular (Array r sh e)

    extent (AIrregular arr) = extent arr

instance Source r e => Source (I r) e where
    index (AIrregular arr) ix =
        index arr ix

    unsafeIndex (AIrregular arr) ix =
        unsafeIndex arr ix

    linearIndex (AIrregular arr) ix =
        linearIndex arr ix

    unsafeLinearIndex (AIrregular arr) ix =
        unsafeLinearIndex arr ix

instance Target r e => Target (I r) e where
    data MArray (I r) sh e = MIrregular (MArray r sh e)

    mextent (MIrregular marr) =
        mextent marr

    newMArray sh =
        MIrregular <$> newMArray sh

    unsafeWriteMArray (MIrregular marr) ix e =
        unsafeWriteMArray marr ix e

    unsafeFreezeMArray (MIrregular marr) =
        AIrregular <$> unsafeFreezeMArray marr

instance Load r sh e => Load (I r) sh e where
    loadP (AIrregular arr) marr = do
        shift $ \k -> do
        p1  <- reset (loadP arr marr >> return (ReturnE UnitE))
        p1' <- checkTraverseFam markLoopsAsIrregular ExpA p1
        p2  <- reset $ k ()
        return $ p1' `seqE` p2

-- Traverse an expression and rewrite all parallel loops into irregular parallel
-- loops.
markLoopsAsIrregular :: MonadCheck m => AST a -> a -> m a
markLoopsAsIrregular ExpA (ForE ParFor loopvs p) =
    ForE IrregParFor <$> (zip is <$> traverse (markLoopsAsIrregular ExpA) es)
                     <*> markLoopsAsIrregular ExpA p
  where
    (is, es) = unzip loopvs

-- The default traversal barfs if it sees a DelayedE, but we don't care.
markLoopsAsIrregular ExpA e@(DelayedE {}) =
    return e

markLoopsAsIrregular w a =
    checkTraverseFam markLoopsAsIrregular w a

hintIrregular :: Array r sh e -> Array (I r) sh e
hintIrregular = AIrregular

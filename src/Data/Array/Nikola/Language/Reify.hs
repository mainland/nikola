{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Data.Array.Nikola.Language.Reify
-- Copyright   : (c) The President and Fellows of Harvard College 2009-2010
-- Copyright   : (c) Geoffrey Mainland 2012
-- License     : BSD-style
--
-- Maintainer  : Geoffrey Mainland <mainland@apeiron.net>
-- Stability   : experimental
-- Portability : non-portable

module Data.Array.Nikola.Language.Reify (
    Reifiable(..),

    delayE,

    vapply
  ) where

import Prelude hiding ((++), map, replicate, reverse)
import qualified Prelude as P

import Control.Applicative
import Data.Typeable (Typeable)
-- import Text.PrettyPrint.Mainland

import Data.Array.Nikola.Array
import Data.Array.Nikola.Exp
import Data.Array.Nikola.Eval
import Data.Array.Nikola.Repr.Global
import Data.Array.Nikola.Shape

import Data.Array.Nikola.Language.Generic
import Data.Array.Nikola.Language.Monad
import Data.Array.Nikola.Language.Sharing
import qualified Data.Array.Nikola.Language.Syntax as S
import Data.Array.Nikola.Language.Syntax hiding (Exp, Var)
-- import Data.Array.Nikola.Pretty

-- | 'Reifiable a b' mean that an 'a' can be reified as a 'b'.
class Typeable a => Reifiable a b where
    reify :: a -> R b b

-- These are the base cases
instance (IsElem (Exp t a)) => Reifiable (Exp t a) S.Exp where
    reify e = return $ unE e

instance Reifiable (P ()) S.Exp where
    reify m = liftK $ m >> return (ReturnE UnitE)

instance (IsElem (Exp t a)) => Reifiable (P (Exp t a)) S.Exp where
    reify m = liftK $ m >>= returnK

instance (Typeable r,
          Shape sh,
          IsElem a,
          Load r sh a)
      => Reifiable (Array r sh a) S.Exp where
    reify arr = liftK $ do
        AGlobal _ arr <- computeP arr
        returnK $ E arr

instance (Shape sh,
          IsElem a)
      => Reifiable (P (Array G sh a)) S.Exp where
    reify m = liftK $ do
        AGlobal _ arr <- m
        returnK $ E arr

liftK :: P S.Exp -> R S.Exp S.Exp
liftK m =
    lamE [] $ do
    LamE [] <$> resetH m

returnK :: Exp t a -> P S.Exp
returnK = return . ReturnE . unE

-- These are the inductive cases

instance (IsElem (Exp t a),
          Reifiable b S.Exp)
    => Reifiable (Exp t a -> b) S.Exp where
    reify f = do
        v <- gensym "x"
        lamE [(v, ScalarT tau)] $ do
        reify $ f (E (VarE v))
      where
        tau :: ScalarType
        tau = typeOf (undefined :: Exp t a)

instance (Shape sh,
          IsElem a,
          Reifiable b S.Exp) => Reifiable (Array G sh a -> b) S.Exp where
    reify f = do
        v        <- gensym "arr"
        let n    =  rank (undefined :: sh)
        let dims =  [DimE i n (VarE v) | i <- [0..n-1]]
        let sh   =  shapeOfList (P.map E dims)
        lamE [(v, ArrayT tau n)] $ do
        reify $ f (AGlobal sh (VarE v))
      where
        tau :: ScalarType
        tau = typeOf (undefined :: a)

instance (Shape sh,
          IsElem a,
          Reifiable b S.Exp) => Reifiable (MArray G sh a -> b) S.Exp where
    reify f = do
        v        <- gensym "marr"
        let n    =  rank (undefined :: sh)
        let dims =  [DimE i n (VarE v) | i <- [0..n-1]]
        let sh   =  shapeOfList (P.map E dims)
        lamE [(v, ArrayT tau n)] $ do
        reify $ f (MGlobal sh (VarE v))
      where
        tau :: ScalarType
        tau = typeOf (undefined :: a)

delayE :: Reifiable a S.Exp => a -> S.Exp
delayE e = DelayedE (cacheExp e (reset (reify e >>= detectSharing ExpA)))

-- | @vapply@ is a bit tricky... We first build a @DelayedE@ AST node containing
-- an action that reifies the lambda. Then we wrap the result in enough
-- (Haskell) lambdas and (Nikola) @AppE@ constructors to turn in back into a
-- Haskell function (at the original type) whose body is a Nikola application
-- term.
class (Reifiable a S.Exp) => VApply a where
    vapply :: a -> a
    vapply f = vapplyk (delayE f) []

    vapplyk :: S.Exp -> [S.Exp] -> a

instance (IsElem (Exp t a),
          IsElem (Exp t b)) => VApply (Exp t a -> Exp t b) where
    vapplyk f es = \e -> E $ AppE f (P.reverse (unE e : es))

instance (IsElem (Exp t a),
          VApply (b -> c)) => VApply (Exp t a -> b -> c) where
    vapplyk f es = \e -> vapplyk f (unE e : es)

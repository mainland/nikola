{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Data.Array.Nikola.Program
-- Copyright   : (c) Geoffrey Mainland 2012
-- License     : BSD-style
--
-- Maintainer  : Geoffrey Mainland <mainland@apeiron.net>
-- Stability   : experimental
-- Portability : non-portable

module Data.Array.Nikola.Program (
    P,

    splitExp,
    isolateK
  ) where

-- import Text.PrettyPrint.Mainland

import Data.Array.Nikola.Exp

import Data.Array.Nikola.Language.Check
import Data.Array.Nikola.Language.Monad
import Data.Array.Nikola.Language.Syntax hiding (Var, Exp)

-- | Evaluate an expression, split the current kernel context, and return the
-- value of the expression in a new kernel context.
splitExp :: forall t a . IsElem (Exp t a)
         => Exp t a
         -> P (Exp t a)
splitExp e = do
    x <- gensym "x"
    shift $ \k -> do
    m2 <- extendVarTypes [(x, ScalarT tau)] $ do
          resetH $ k (E (VarE x))
    shiftH $ \k -> do
        m1  <- reset $ k ()
        tau <- inferProgH m1
        return $ bindH x tau m1 m2
    return $ ReturnK (unE e)
  where
    tau :: ScalarType
    tau = typeOf (undefined :: Exp t a)

-- | Isolate a kernel program, running it in a completely separate kernel.
isolateK :: ProgK -> P ProgK
isolateK p = do
    shift $ \k -> do
    m1 <- resetH $ k (ReturnK UnitE)
    shiftH $ \_ -> do
        return $ m1 `seqH` LiftH (ProcK [] p) []
    return $ ReturnK UnitE

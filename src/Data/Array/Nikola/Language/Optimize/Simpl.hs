{-# LANGUAGE GADTs #-}

-- |
-- Module      : Data.Array.Nikola.Language.Optimize.Simpl
-- Copyright   : (c) Geoffrey Mainland 2012
-- License     : BSD-style
--
-- Maintainer  : Geoffrey Mainland <mainland@apeiron.net>
-- Stability   : experimental
-- Portability : non-portable

module Data.Array.Nikola.Language.Optimize.Simpl (simpl) where

import Prelude hiding (mapM)

import Control.Applicative (Applicative, (<$>), (<*>), pure)

import Data.Array.Nikola.Language.Generic
import Data.Array.Nikola.Language.Optimize.Monad
import Data.Array.Nikola.Language.Optimize.Subst
import Data.Array.Nikola.Language.Syntax

simpl :: AST a -> a -> O a
simpl ExpA (VarE v) = lookupSubst VarA v ExpA (return (VarE v))

simpl ExpA (LetE _ _ Never _ e2) =
    simpl ExpA e2

simpl ExpA (LetE v _ Once e1 e2) = do
    e1' <- simpl ExpA e1
    insertSubst VarA v ExpA e1'
    simpl ExpA e2

simpl ExpA (LetE v tau Many e1 e2) =
    LetE v tau Many <$> simpl ExpA e1 <*> simpl ExpA e2

simpl ExpA (UnopE op e) = do  e' <- simpl ExpA e
                              go op e'
  where
    go :: Unop -> Exp -> O Exp
    go SqrtF  (ConstE (FloatC i)) = pure $ ConstE (FloatC (sqrt i))
    go RecipF (ConstE (FloatC i)) = pure $ ConstE (FloatC (1/i))

    go SqrtF  (ConstE (DoubleC i)) = pure $ ConstE (DoubleC (sqrt i))
    go RecipF (ConstE (DoubleC i)) = pure $ ConstE (DoubleC (1/i))

    go op e = return $ UnopE op e

simpl ExpA (BinopE op e1 e2) = do  e1' <- simpl ExpA e1
                                   e2' <- simpl ExpA e2
                                   go op e1' e2'
  where
    go :: Binop -> Exp -> Exp -> O Exp
    go AddN (ConstE (Int32C 0))  e2                   = pure e2
    go AddN (ConstE (FloatC 0))  e2                   = pure e2
    go AddN (ConstE (DoubleC 0)) e2                   = pure e2
    go AddN (ConstE (Int32C i))  (ConstE (Int32C j))  = pure $ ConstE (Int32C (i+j))
    go AddN (ConstE (FloatC i))  (ConstE (FloatC j))  = pure $ ConstE (FloatC (i+j))
    go AddN (ConstE (DoubleC i)) (ConstE (DoubleC j)) = pure $ ConstE (DoubleC (i+j))
    go AddN e1                   (BinopE AddN e2 e3)  = simpl ExpA (BinopE AddN (BinopE AddN e1 e2) e3)
    go AddN e1                   e2@(ConstE {})       = simpl ExpA (BinopE AddN e2 e1)

    go SubN e1                  (ConstE (Int32C i))   = simpl ExpA (BinopE AddN (ConstE (Int32C (negate i))) e1)
    go SubN e1                  (ConstE (FloatC i))   = simpl ExpA (BinopE AddN (ConstE (FloatC (negate i))) e1)
    go SubN e1                  (ConstE (DoubleC i))  = simpl ExpA (BinopE AddN (ConstE (DoubleC (negate i))) e1)

    go MulN (ConstE (Int32C 0))  _                    = pure $ ConstE (Int32C 0)
    go MulN (ConstE (FloatC 0))  _                    = pure $ ConstE (FloatC 0)
    go MulN (ConstE (DoubleC 0)) _                    = pure $ ConstE (DoubleC 0)
    go MulN (ConstE (Int32C 1))  e2                   = pure e2
    go MulN (ConstE (FloatC 1))  e2                   = pure e2
    go MulN (ConstE (DoubleC 1)) e2                   = pure e2
    go MulN (ConstE (Int32C i))  (ConstE (Int32C j))  = pure $ ConstE (Int32C (i*j))
    go MulN (ConstE (FloatC i))  (ConstE (FloatC j))  = pure $ ConstE (FloatC (i*j))
    go MulN (ConstE (DoubleC i)) (ConstE (DoubleC j)) = pure $ ConstE (DoubleC (i*j))
    go MulN e1                   e2@(ConstE {})       = simpl ExpA (BinopE MulN e2 e1)
    go MulN e1                   (BinopE MulN e2 e3)  = simpl ExpA (BinopE MulN (BinopE MulN e1 e2) e3)

    go ModI e1                  (ConstE (Int32C 1))   = pure e1

    go DivN (ConstE (FloatC 1)) e2                    = simpl ExpA (UnopE RecipF e2)
    go DivN (ConstE (DoubleC 1)) e2                   = simpl ExpA (UnopE RecipF e2)

    -- Default
    go op e1 e2 = return $ BinopE op e1 e2

simpl ExpA (CallE (LamE [] (SeqE m1 (ReturnE e))) []) =
    simpl ExpA (SeqE (CallE (LamE [] m1) []) (ReturnE e))

simpl w a = traverseFam simpl w a

{-
isAtomic :: Exp -> Bool
isAtomic (VarE {})        = True
isAtomic (ConstE {})      = True
isAtomic (ProjArrE _ _ e) = isAtomic e
isAtomic (DimE _ _ e)     = isAtomic e
isAtomic (UnopE NegN _)   = True
isAtomic _                = False
-}

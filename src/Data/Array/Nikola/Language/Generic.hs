{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : Data.Array.Nikola.Language.Generic
-- Copyright   : (c) Geoffrey Mainland 2012
-- License     : BSD-style
--
-- Maintainer  : Geoffrey Mainland <mainland@apeiron.net>
-- Stability   : experimental
-- Portability : non-portable

module Data.Array.Nikola.Language.Generic (
    module Data.Array.Nikola.Util.Generic,
    AST(..)
  ) where

import Control.Applicative (Applicative, (<$>), (<*>), pure)
import Data.Traversable
import Text.PrettyPrint.Mainland

import Data.Array.Nikola.Language.Syntax
import Data.Array.Nikola.Util.Generic

data AST a where
    VarA        :: AST Var
    ScalarTypeA :: AST ScalarType
    TypeA       :: AST Type
    ExpA        :: AST Exp

instance TraversableFamily AST where
    traverseFam (trav :: Traversal AST f) = go
      where
        go :: Traversal AST f
        go VarA v = pure v

        go ScalarTypeA tau@UnitT     = pure tau
        go ScalarTypeA tau@BoolT     = pure tau
        go ScalarTypeA tau@Int8T     = pure tau
        go ScalarTypeA tau@Int16T    = pure tau
        go ScalarTypeA tau@Int32T    = pure tau
        go ScalarTypeA tau@Int64T    = pure tau
        go ScalarTypeA tau@Word8T    = pure tau
        go ScalarTypeA tau@Word16T   = pure tau
        go ScalarTypeA tau@Word32T   = pure tau
        go ScalarTypeA tau@Word64T   = pure tau
        go ScalarTypeA tau@FloatT    = pure tau
        go ScalarTypeA tau@DoubleT   = pure tau
        go ScalarTypeA (TupleT taus) = TupleT <$> traverse (trav ScalarTypeA) taus

        go TypeA (ScalarT tau)   = ScalarT <$> trav ScalarTypeA tau
        go TypeA (ArrayT tau n)  = ArrayT <$> trav ScalarTypeA tau <*> pure n
        go TypeA (FunT taus tau) = FunT <$> traverse (trav TypeA) taus <*> trav TypeA tau
        go TypeA (MT tau)        = MT <$> trav TypeA tau

        go ExpA (VarE v)                = VarE <$> trav VarA v
        go ExpA e@(ConstE {})           = pure e
        go ExpA e@(UnitE {})            = pure e

        go ExpA (TupleE es)             = TupleE <$> traverse (trav ExpA) es
        go ExpA (ProjE i n e)           = ProjE i n <$> trav ExpA e

        go ExpA (LetE v t occ e1 e2)    = LetE v t occ <$> trav ExpA e1 <*> trav ExpA e2

        go ExpA (LamE vtaus e)          = LamE vtaus <$> trav ExpA e
        go ExpA (AppE e es)             = AppE <$> trav ExpA e <*> traverse (trav ExpA) es
        go ExpA (CallE e es)            = CallE <$> trav ExpA e <*> traverse (trav ExpA) es

        go ExpA (UnopE op e)            = UnopE op <$> trav ExpA e
        go ExpA (BinopE op e1 e2)       = BinopE op <$> trav ExpA e1 <*> trav ExpA e2

        go ExpA (IfThenElseE e1 e2 e3)  = IfThenElseE <$> trav ExpA e1
                                                      <*> trav ExpA e2
                                                      <*> trav ExpA e3

        go ExpA (SwitchE e cases dflt)  = let f (i, e) = (,) i <$> trav ExpA e
                                          in
                                            SwitchE <$> trav ExpA e
                                                    <*> traverse f cases
                                                    <*> traverse (trav ExpA) dflt

        go ExpA (ReturnE e)             = ReturnE <$> trav ExpA e
        go ExpA (SeqE p1 p2)            = SeqE <$> trav ExpA p1 <*> trav ExpA p2
        go ExpA (ParE p1 p2)            = ParE <$> trav ExpA p1 <*> trav ExpA p2
        go ExpA (BindE v tau p1 p2)     = BindE v tau <$> trav ExpA p1 <*> trav ExpA p2

        go ExpA (AllocE tau sh)         = AllocE tau <$> traverse (trav ExpA) sh
        go ExpA (DimE i n e)            = DimE i n <$> trav ExpA e
        go ExpA (ProjArrE i n e)        = ProjArrE i n <$> trav ExpA e
        go ExpA (IndexE v idx)          = IndexE <$> trav ExpA v <*> trav ExpA idx
        go ExpA (WriteE v idx e)        = WriteE <$> trav ExpA v
                                                 <*> trav ExpA idx
                                                 <*> trav ExpA e

        go ExpA (IterateE f n x)        = IterateE <$> trav ExpA f
                                                   <*> trav ExpA n
                                                   <*> trav ExpA x

        go ExpA (IterateWhileE f n x)   = IterateWhileE <$> trav ExpA f
                                                        <*> trav ExpA n
                                                        <*> trav ExpA x

        go ExpA (ForE isPar vs es p)    = ForE isPar vs <$> traverse (trav ExpA) es
                                                        <*> trav ExpA p

        go ExpA SyncE                   = pure SyncE

        go ExpA e@(DelayedE {})         = errordoc $ text "Cannot traverse:" <+> ppr e

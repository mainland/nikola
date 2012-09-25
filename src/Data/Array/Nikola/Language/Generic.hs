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
    ProgKA      :: AST ProgK
    ProcKA      :: AST ProcK
    ProgHA      :: AST ProgH
    ProcHA      :: AST ProcH

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
        go ExpA (ProjArrE i n e)        = ProjArrE i n <$> trav ExpA e
        go ExpA (DimE i n e)            = DimE i n <$> trav ExpA e
        go ExpA (LetE v t occ e1 e2)    = LetE v t occ <$> trav ExpA e1 <*> trav ExpA e2
        go ExpA (LamE vtaus e)          = LamE vtaus <$> trav ExpA e
        go ExpA (AppE e es)             = AppE <$> trav ExpA e <*> traverse (trav ExpA) es
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
        go ExpA (IndexE v idx)          = IndexE <$> trav ExpA v <*> trav ExpA idx
        go ExpA e@(DelayedE {})         = errordoc $ text "Cannot traverse:" <+> ppr e

        go ProgKA (ReturnK e)           = ReturnK <$> trav ExpA e
        go ProgKA (SeqK p1 p2)          = SeqK <$> trav ProgKA p1 <*> trav ProgKA p2
        go ProgKA (ParK p1 p2)          = ParK <$> trav ProgKA p1 <*> trav ProgKA p2
        go ProgKA (LetK v tau e p)      = LetK v tau <$> trav ExpA e <*> trav ProgKA p
        go ProgKA (BindK v tau p1 p2)   = BindK v tau <$> trav ProgKA p1 <*> trav ProgKA p2
        go ProgKA (ForK vs es p)        = ForK vs <$> traverse (trav ExpA) es <*> trav ProgKA p
        go ProgKA (ParforK vs es p)     = ParforK vs <$> traverse (trav ExpA) es <*> trav ProgKA p
        go ProgKA (IfThenElseK e p1 p2) = IfThenElseK <$> trav ExpA e
                                                      <*> trav ProgKA p1
                                                      <*> trav ProgKA p2
        go ProgKA (WriteK v idx e)      = WriteK <$> trav ExpA v
                                                 <*> trav ExpA idx
                                                 <*> trav ExpA e
        go ProgKA SyncK                 = pure SyncK
        go ProgKA p@(DelayedK {})       = errordoc $ text "Cannot traverse:" <+> ppr p

        go ProcKA (ProcK vtaus p)       = ProcK vtaus <$> trav ProgKA p

        go ProgHA (ReturnH e)           = ReturnH <$> trav ExpA e
        go ProgHA (SeqH p1 p2)          = SeqH <$> trav ProgHA p1 <*> trav ProgHA p2
        go ProgHA (LetH v tau e p)      = LetH v tau <$> trav ExpA e <*> trav ProgHA p
        go ProgHA (BindH v tau p1 p2)   = BindH v tau <$> trav ProgHA p1 <*> trav ProgHA p2
        go ProgHA (LiftH p es)          = LiftH <$> trav ProcKA p <*> traverse (trav ExpA) es
        go ProgHA (IfThenElseH e p1 p2) = IfThenElseH <$> trav ExpA e
                                                      <*> trav ProgHA p1
                                                      <*> trav ProgHA p2
        go ProgHA (AllocH tau sh)       = AllocH tau <$> traverse (trav ExpA) sh
        go ProgHA p@(DelayedH {})       = errordoc $ text "Cannot traverse:" <+> ppr p

        go ProcHA (ProcH vtaus p)       = ProcH vtaus <$> trav ProgHA p

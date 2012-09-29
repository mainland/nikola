{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

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
import Text.PrettyPrint.Mainland

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
    go EqO (ConstE c1) (ConstE c2) = pure $ ConstE (liftOrd2 (==) c1 c2)
    go NeO (ConstE c1) (ConstE c2) = pure $ ConstE (liftOrd2 (/=) c1 c2)
    go GtO (ConstE c1) (ConstE c2) = pure $ ConstE (liftOrd2 (>)  c1 c2)
    go GeO (ConstE c1) (ConstE c2) = pure $ ConstE (liftOrd2 (>=) c1 c2)
    go LtO (ConstE c1) (ConstE c2) = pure $ ConstE (liftOrd2 (<)  c1 c2)
    go LeO (ConstE c1) (ConstE c2) = pure $ ConstE (liftOrd2 (<=) c1 c2)

    go MaxO (ConstE c1) (ConstE c2) = pure $ ConstE (liftMinMax2 max c1 c2)
    go MinO (ConstE c1) (ConstE c2) = pure $ ConstE (liftMinMax2 min c1 c2)

    go AddN (ConstE c)  e2
        | c `equalTo` 0                      = pure e2
    go AddN (ConstE i)  (ConstE j)           = pure $ ConstE (liftNum2 (+) i j)
    go AddN e1          (BinopE AddN e2 e3)  = simpl ExpA (BinopE AddN (BinopE AddN e1 e2) e3)
    go AddN e1          e2@(ConstE {})       = simpl ExpA (BinopE AddN e2 e1)

    go SubN (ConstE i)  (ConstE j)           = pure $ ConstE (liftNum2 (-) i j)
    go SubN e1          (ConstE c)
        | c `equalTo` 0                      = pure e2
        | otherwise                          = simpl ExpA (BinopE AddN (ConstE (liftNum negate c)) e1)

    go MulN (ConstE c)  e2
        | c `equalTo` 0                      = pure $ ConstE (liftNum (const 0) c)
        | c `equalTo` 1                      = pure e2
    go MulN (ConstE i)  (ConstE j)           = pure $ ConstE (liftNum2 (*) i j)
    go MulN e1          e2@(ConstE {})       = simpl ExpA (BinopE MulN e2 e1)
    go MulN e1          (BinopE MulN e2 e3)  = simpl ExpA (BinopE MulN (BinopE MulN e1 e2) e3)

    go QuotI e1         (ConstE c)
        | c `equalTo` 1                      = pure e1
    go RemI e1          (ConstE c)
        | c `equalTo` 1                      = pure e1

    go DivF (ConstE c) e2
        | c `equalTo` 1                      = simpl ExpA (UnopE RecipF e2)

    -- Default
    go op e1 e2 = return $ BinopE op e1 e2

simpl ExpA (CallE (LamE [] (SeqE m1 (ReturnE e))) []) =
    simpl ExpA (SeqE (CallE (LamE [] m1) []) (ReturnE e))

simpl w a = traverseFam simpl w a

equalTo :: Const -> Integer -> Bool
equalTo (Int8C n1)   n2 = n1 == fromIntegral n2
equalTo (Int16C n1)  n2 = n1 == fromIntegral n2
equalTo (Int32C n1)  n2 = n1 == fromIntegral n2
equalTo (Int64C n1)  n2 = n1 == fromIntegral n2
equalTo (Word8C n1)  n2 = n1 == fromIntegral n2
equalTo (Word16C n1) n2 = n1 == fromIntegral n2
equalTo (Word32C n1) n2 = n1 == fromIntegral n2
equalTo (Word64C n1) n2 = n1 == fromIntegral n2
equalTo (FloatC n1)  n2 = n1 == fromIntegral n2
equalTo (DoubleC n1) n2 = n1 == fromIntegral n2
equalTo c            n  = errordoc $
                          text "internal error: equalTo:" <+>
                          ppr c <+> ppr n

liftOrd2 :: (forall a . Ord a => a -> a -> Bool)
         -> Const -> Const -> Const
liftOrd2 op c1 c2 =
    go c1 c2
  where
    go :: Const -> Const -> Const
    go (BoolC c1)   (BoolC c2)   = BoolC (op c1 c2)
    go (Int8C c1)   (Int8C c2)   = BoolC (op c1 c2)
    go (Int16C c1)  (Int16C c2)  = BoolC (op c1 c2)
    go (Int32C c1)  (Int32C c2)  = BoolC (op c1 c2)
    go (Int64C c1)  (Int64C c2)  = BoolC (op c1 c2)
    go (Word8C c1)  (Word8C c2)  = BoolC (op c1 c2)
    go (Word16C c1) (Word16C c2) = BoolC (op c1 c2)
    go (Word32C c1) (Word32C c2) = BoolC (op c1 c2)
    go (Word64C c1) (Word64C c2) = BoolC (op c1 c2)
    go (FloatC c1)  (FloatC c2)  = BoolC (op c1 c2)
    go (DoubleC c1) (DoubleC c2) = BoolC (op c1 c2)
    go c1           c2           = errordoc $
                                   text "internal error: liftOrd2:" <+>
                                   ppr c1 <+> ppr c2

liftMinMax2 :: (forall a . Ord a => a -> a -> a)
            -> Const -> Const -> Const
liftMinMax2 op c1 c2 =
    go c1 c2
  where
    go :: Const -> Const -> Const
    go (BoolC c1)   (BoolC c2)   = BoolC   (op c1 c2)
    go (Int8C c1)   (Int8C c2)   = Int8C   (op c1 c2)
    go (Int16C c1)  (Int16C c2)  = Int16C  (op c1 c2)
    go (Int32C c1)  (Int32C c2)  = Int32C  (op c1 c2)
    go (Int64C c1)  (Int64C c2)  = Int64C  (op c1 c2)
    go (Word8C c1)  (Word8C c2)  = Word8C  (op c1 c2)
    go (Word16C c1) (Word16C c2) = Word16C (op c1 c2)
    go (Word32C c1) (Word32C c2) = Word32C (op c1 c2)
    go (Word64C c1) (Word64C c2) = Word64C (op c1 c2)
    go (FloatC c1)  (FloatC c2)  = FloatC  (op c1 c2)
    go (DoubleC c1) (DoubleC c2) = DoubleC (op c1 c2)
    go c1           c2           = errordoc $
                                   text "internal error: liftMinMax2:" <+>
                                   ppr c1 <+> ppr c2

liftNum :: (forall a . Num a => a -> a)
        -> Const -> Const
liftNum op c =
    go c
  where
    go :: Const -> Const
    go (Int8C c)   = Int8C   (op c)
    go (Int16C c)  = Int16C  (op c)
    go (Int32C c)  = Int32C  (op c)
    go (Int64C c)  = Int64C  (op c)
    go (Word8C c)  = Word8C  (op c)
    go (Word16C c) = Word16C (op c)
    go (Word32C c) = Word32C (op c)
    go (Word64C c) = Word64C (op c)
    go (FloatC c)  = FloatC  (op c)
    go (DoubleC c) = DoubleC (op c)
    go c           = errordoc $
                     text "internal error: liftNum:" <+>
                     ppr c

liftNum2 :: (forall a . Num a => a -> a -> a)
         -> Const -> Const -> Const
liftNum2 op c1 c2 =
    go c1 c2
  where
    go :: Const -> Const -> Const
    go (Int8C c1)   (Int8C c2)   = Int8C   (op c1 c2)
    go (Int16C c1)  (Int16C c2)  = Int16C  (op c1 c2)
    go (Int32C c1)  (Int32C c2)  = Int32C  (op c1 c2)
    go (Int64C c1)  (Int64C c2)  = Int64C  (op c1 c2)
    go (Word8C c1)  (Word8C c2)  = Word8C  (op c1 c2)
    go (Word16C c1) (Word16C c2) = Word16C (op c1 c2)
    go (Word32C c1) (Word32C c2) = Word32C (op c1 c2)
    go (Word64C c1) (Word64C c2) = Word64C (op c1 c2)
    go (FloatC c1)  (FloatC c2)  = FloatC  (op c1 c2)
    go (DoubleC c1) (DoubleC c2) = DoubleC (op c1 c2)
    go c1           c2           = errordoc $
                                   text "internal error: liftNum2:" <+>
                                   ppr c1 <+> ppr c2

{-
liftIntegral2 :: (forall a . Integral a => a -> a -> a)
              -> Const -> Const -> Const
liftIntegral2 op c1 c2 =
    go c1 c2
  where
    go :: Const -> Const -> Const
    go (Int8C c1)   (Int8C c2)   = Int8C   (op c1 c2)
    go (Int16C c1)  (Int16C c2)  = Int16C  (op c1 c2)
    go (Int32C c1)  (Int32C c2)  = Int32C  (op c1 c2)
    go (Int64C c1)  (Int64C c2)  = Int64C  (op c1 c2)
    go (Word8C c1)  (Word8C c2)  = Word8C  (op c1 c2)
    go (Word16C c1) (Word16C c2) = Word16C (op c1 c2)
    go (Word32C c1) (Word32C c2) = Word32C (op c1 c2)
    go (Word64C c1) (Word64C c2) = Word64C (op c1 c2)
    go c1           c2           = errordoc $
                                   text "internal error: liftIntegral2:" <+>
                                   ppr c1 <+> ppr c2

liftFractional2 :: (forall a . Fractional a => a -> a -> a)
                -> Const -> Const -> Const
liftFractional2 op c1 c2 =
    go c1 c2
  where
    go :: Const -> Const -> Const
    go (FloatC c1)  (FloatC c2)  = FloatC  (op c1 c2)
    go (DoubleC c1) (DoubleC c2) = DoubleC (op c1 c2)
    go c1           c2           = errordoc $
                                   text "internal error: liftFractional2:" <+>
                                   ppr c1 <+> ppr c2

liftFloating2 :: (forall a . Floating a => a -> a -> a)
              -> Const -> Const -> Const
liftFloating2 op c1 c2 =
    go c1 c2
  where
    go :: Const -> Const -> Const
    go (FloatC c1)  (FloatC c2)  = FloatC  (op c1 c2)
    go (DoubleC c1) (DoubleC c2) = DoubleC (op c1 c2)
    go c1           c2           = errordoc $
                                   text "internal error: liftFloating2:" <+>
                                   ppr c1 <+> ppr c2
-}

{-
isAtomic :: Exp -> Bool
isAtomic (VarE {})        = True
isAtomic (ConstE {})      = True
isAtomic (ProjArrE _ _ e) = isAtomic e
isAtomic (DimE _ _ e)     = isAtomic e
isAtomic (UnopE NegN _)   = True
isAtomic _                = False
-}

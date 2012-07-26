{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : Data.Array.Nikola.Exp
-- Copyright   : (c) Geoffrey Mainland 2012
-- License     : BSD-style
--
-- Maintainer  : Geoffrey Mainland <mainland@apeiron.net>
-- Stability   : experimental
-- Portability : non-portable

module Data.Array.Nikola.Exp (
    Ix,
    Var(..),
    Exp(..),

    IfThenElse(..),
    (?),

    -- * Comparison operators
    (.<.), (.<=.), (.==.), (./=.), (.>.), (.>=.),

    -- * Boolean operators
    (.&&.), (.||.),

    -- * Bitwise operators
    (.&.), (.|.),

    -- * Helpers
    varE, voidE,

    IsIntegral(..),

    Lift(..),

    Ptr,
    IsElem(..),
    indexScalar,
    writeScalar
  ) where

import Data.Bits (Bits)
import Data.Int
import Data.Typeable (Typeable)

import Data.Array.Nikola.Language.Monad
import qualified Data.Array.Nikola.Language.Syntax as S
import Data.Array.Nikola.Language.Syntax hiding (Exp, Var)

-- | Type of array indices
type Ix = Int32

-- | Embedded language variables
newtype Var t a = V { unV :: S.Var }
  deriving (Typeable)

instance Eq a => Eq (Var t a) where
    _ == _ = error "Var: incomparable"

instance Ord a => Ord (Var t a) where
    compare _  _ = error "Var: incomparable"

-- | Embedded language expressions
newtype Exp t a = E { unE :: S.Exp }
  deriving (Typeable)

instance Eq a => Eq (Exp t a) where
    _ == _ = error "Exp: incomparable"

instance Ord a => Ord (Exp t a) where
    compare _  _ = error "Exp: incomparable"

unop :: (S.Exp -> S.Exp) -> Exp t a -> Exp t b
unop op e = (E . op . unE) e

binop :: (S.Exp -> S.Exp -> S.Exp) -> Exp t a -> Exp t b -> Exp t c
binop op e1 e2 = E $ op (unE e1) (unE e2)

-- | A class that defines 'ifThenElse' so we can use rebindable syntax.
class IfThenElse a b where
    ifThenElse :: a -> b -> b -> b

instance IfThenElse Bool a where
    ifThenElse True  th _  = th
    ifThenElse False _  el = el

instance IfThenElse (Exp t Bool) (Exp t a) where
    ifThenElse t th el = E $ IfThenElseE (unE t) (unE th) (unE el)

instance (IfThenElse (Exp t Bool) a, IfThenElse (Exp t Bool) b)
    => IfThenElse (Exp t Bool) (a, b) where
    ifThenElse t (tha, thb) (ela, elb) =
        (ifThenElse t tha ela, ifThenElse t thb elb)

-- | A binary if-then-else combinator.
(?) :: Exp t Bool -> (Exp t a, Exp t a) -> Exp t a
t ? (th, el) = E $ IfThenElseE (unE t) (unE th) (unE el)

(.==.), (./=.) :: Eq a => Exp t a -> Exp t a -> Exp t Bool
(.==.) = binop (BinopE Leq)
(./=.) = binop (BinopE Lne)

(.<.), (.<=.), (.>.), (.>=.) :: Ord a => Exp t a -> Exp t a -> Exp t Bool
(.<.)  = binop (BinopE Llt)
(.<=.) = binop (BinopE Lle)
(.>.)  = binop (BinopE Lgt)
(.>=.) = binop (BinopE Lge)

-- | Boolean operators
infixr 3  .&&.
infixr 2  .||.

(.&&.) :: Exp t Bool -> Exp t Bool -> Exp t Bool
e1 .&&. e2 = binop (BinopE Land) e1 e2

(.||.) :: Exp t Bool -> Exp t Bool -> Exp t Bool
e1 .||. e2 = binop (BinopE Land) e1 e2

-- | Bit-wise operators
infixl 7 .&.
infixl 5 .|.

(.&.) :: Bits a => Exp t a -> Exp t a -> Exp t a
e1 .&. e2 = binop (BinopE Band) e1 e2

(.|.) :: Bits a => Exp t a -> Exp t a -> Exp t a
e1 .|. e2 = binop (BinopE Bor) e1 e2

-- | Helpers
varE :: Var t a -> Exp t a
varE = E . VarE . unV

voidE :: Exp t ()
voidE = E UnitE

class IsIntegral a b where
    fromInt :: a -> b

-- Int32
instance Num (Exp t Int32) where
    e1 + e2 = binop (BinopE Iadd) e1 e2
    e1 - e2 = binop (BinopE Isub) e1 e2
    e1 * e2 = binop (BinopE Imul) e1 e2

    negate e    = unop (UnopE Ineg) e
    fromInteger = E . ConstE . Int32C . fromInteger

    abs e    = unop (UnopE Iabs) e
    signum e = unop (UnopE Isignum) e

instance Real (Exp t Int32) where
    toRational _ = error "Exp: cannot convert to rational"

instance Enum (Exp t Int32) where
    toEnum _   = error "Exp: not enumerable"
    fromEnum _ = error "Exp: not enumerable"

instance Integral (Exp t Int32) where
    quot = binop (BinopE Idiv)
    rem  = binop (BinopE Imod)

    quotRem e1 e2 = (quot e1 e2, rem e1 e2)

    toInteger _ = error "Exp: cannot convert to Integer"

-- Float
instance IsIntegral (Exp t Int32) (Exp t Float) where
    fromInt = unop (UnopE Itof)

instance Num (Exp t Float) where
    e1 + e2 = binop (BinopE Fadd) e1 e2
    e1 - e2 = binop (BinopE Fsub) e1 e2
    e1 * e2 = binop (BinopE Fmul) e1 e2

    negate e    = unop (UnopE Fneg) e
    fromInteger = E . ConstE . FloatC . fromInteger

    abs e    = unop (UnopE Fabs) e
    signum e = unop (UnopE Fsignum) e

instance Real (Exp t Float) where
    toRational _ = error "Exp: cannot convert to rational"

instance Fractional (Exp t Float) where
    (/) = binop (BinopE Fdiv)

    recip (E (UnopE Frecip e)) = E e
    recip e                    = unop (UnopE Frecip) e

    fromRational = E . ConstE . FloatC . fromRational

instance Floating (Exp t Float) where
    pi      = (E . ConstE . FloatC) pi
    exp     = unop (UnopE Fexp)
    sqrt    = unop (UnopE Fsqrt)
    log     = unop (UnopE Flog)
    (**)    = binop (BinopE Fpow)
    logBase = binop (BinopE FlogBase)
    sin     = unop (UnopE Fsin)
    tan     = unop (UnopE Ftan)
    cos     = unop (UnopE Fcos)
    asin    = unop (UnopE Fasin)
    atan    = unop (UnopE Fatan)
    acos    = unop (UnopE Facos)
    sinh    = unop (UnopE Fsinh)
    tanh    = unop (UnopE Ftanh)
    cosh    = unop (UnopE Fcosh)
    asinh   = unop (UnopE Fasinh)
    atanh   = unop (UnopE Fatanh)
    acosh   = unop (UnopE Facosh)

-- Double
instance IsIntegral (Exp t Int32) (Exp t Double) where
    fromInt = unop (UnopE Itod)

instance Num (Exp t Double) where
    e1 + e2 = binop (BinopE Dadd) e1 e2
    e1 - e2 = binop (BinopE Dsub) e1 e2
    e1 * e2 = binop (BinopE Dmul) e1 e2

    negate e    = unop (UnopE Dneg) e
    fromInteger = E . ConstE . DoubleC . fromInteger

    abs e    = unop (UnopE Dabs) e
    signum e = unop (UnopE Dsignum) e

instance Real (Exp t Double) where
    toRational _ = error "Exp: cannot convert to rational"

instance Fractional (Exp t Double) where
    (/) = binop (BinopE Ddiv)

    recip (E (UnopE Drecip e)) = E e
    recip e                    = unop (UnopE Drecip) e

    fromRational = E . ConstE . DoubleC . fromRational

instance Floating (Exp t Double) where
    pi      = (E . ConstE . DoubleC) pi
    exp     = unop (UnopE Dexp)
    sqrt    = unop (UnopE Dsqrt)
    log     = unop (UnopE Dlog)
    (**)    = binop (BinopE Dpow)
    logBase = binop (BinopE DlogBase)
    sin     = unop (UnopE Dsin)
    tan     = unop (UnopE Dtan)
    cos     = unop (UnopE Dcos)
    asin    = unop (UnopE Dasin)
    atan    = unop (UnopE Datan)
    acos    = unop (UnopE Dacos)
    sinh    = unop (UnopE Dsinh)
    tanh    = unop (UnopE Dtanh)
    cosh    = unop (UnopE Dcosh)
    asinh   = unop (UnopE Dasinh)
    atanh   = unop (UnopE Datanh)
    acosh   = unop (UnopE Dacosh)

class Lift a where
    lift :: a -> Exp t a

instance Lift Bool where
    lift = E . ConstE . BoolC

instance Lift Int32 where
    lift = E . ConstE . Int32C

instance Lift Float where
    lift = E . ConstE . FloatC

instance Lift Double where
    lift = E . ConstE . DoubleC

data Ptr a

-- | 'IsElem t a' means that 'a' is a scalar value on target 't'.
class (Typeable a) => IsElem a where
    -- | 'Rep a' is the /representation/ type for 'a' when a value of type 'a'
    -- is tranferred to the target.
    type Rep a :: *

    -- | The embedded type that corresponds to 'a'.
    typeOf :: a -> ScalarType

    indexElem :: Exp t (Ptr a) -> Exp t Ix -> a
    writeElem :: Exp t (Ptr a) -> Exp t Ix -> a -> P ()

indexScalar :: S.Exp -> S.Exp -> S.Exp
indexScalar arr ix =
    IndexE arr ix

writeScalar :: S.Exp -> S.Exp -> S.Exp -> P ()
writeScalar arr ix x =
        shift $ \k -> do
        let p1 =  WriteK arr ix x
        p2     <- reset $ k ()
        return $ p1 `seqK` p2

proj :: Int -> Int -> Exp t a -> Exp t b
proj i n tup = E (ProjE i n (unE tup))

projArr :: Int -> Int -> Exp t a -> Exp t b
projArr i n tup = E (ProjArrE i n (unE tup))

instance ( IsElem a
         , IsElem b
         ) => IsElem (a, b) where
    type Rep (a, b) = (Rep a, Rep b)

    typeOf _ = TupleT [ typeOf (undefined :: a)
                      , typeOf (undefined :: b)
                      ]

    indexElem arr ix =
        ( indexElem (projArr 0 2 arr) ix
        , indexElem (projArr 1 2 arr) ix
        )

    writeElem arr ix (a, b) = do
        writeElem (projArr 0 2 arr) ix a
        writeElem (projArr 1 2 arr) ix b

instance ( Typeable t
         , Typeable a
         , IsElem (Exp t Bool)
         , IsElem (Exp t a)
         ) => IsElem (Exp t (Maybe a)) where
    type Rep (Exp t (Maybe a)) = (Rep (Exp t Bool), Rep (Exp t a))

    typeOf _ = TupleT [ typeOf (undefined :: (Exp t Bool))
                      , typeOf (undefined :: (Exp t a))
                      ]

    indexElem arr ix =
        E $ TupleE [ unE (indexElem (proj 0 2 arr) ix :: Exp t Bool)
                   , unE (indexElem (proj 1 2 arr) ix :: Exp t a)
                   ]

    writeElem arr ix maybe_a = do
        writeElem (projArr 0 2 arr) ix (proj 0 2 maybe_a :: Exp t Bool)
        writeElem (projArr 1 2 arr) ix (proj 1 2 maybe_a :: Exp t a)

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

    -- * Numerical operators
    (.^.),

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
    _ == _ = error " (==) Exp: incomparable"
    _ /= _ = error " (/=) Exp: incomparable"

instance Ord a => Ord (Exp t a) where
    compare _ _ = error "compare Exp: incomparable"

    _ <  _  = error "(<) Exp: incomparable"
    _ <= _  = error "(<=) Exp: incomparable"
    _ >= _  = error "(>=) Exp: incomparable"
    _ >  _  = error "(>) Exp: incomparable"

    max = binop (BinopE MaxO)
    min = binop (BinopE MinO)

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
(.==.) = binop (BinopE EqO)
(./=.) = binop (BinopE NeO)

(.<.), (.<=.), (.>.), (.>=.) :: Ord a => Exp t a -> Exp t a -> Exp t Bool
(.<.)  = binop (BinopE LtO)
(.<=.) = binop (BinopE LeO)
(.>.)  = binop (BinopE GtO)
(.>=.) = binop (BinopE GeO)

-- | Boolean operators
infixr 3  .&&.
infixr 2  .||.

(.&&.) :: Exp t Bool -> Exp t Bool -> Exp t Bool
e1 .&&. e2 = binop (BinopE AndL) e1 e2

(.||.) :: Exp t Bool -> Exp t Bool -> Exp t Bool
e1 .||. e2 = binop (BinopE OrL) e1 e2

-- | Bit-wise operators
infixl 7 .&.
infixl 5 .|.

(.&.) :: Bits a => Exp t a -> Exp t a -> Exp t a
e1 .&. e2 = binop (BinopE AndB) e1 e2

(.|.) :: Bits a => Exp t a -> Exp t a -> Exp t a
e1 .|. e2 = binop (BinopE OrB) e1 e2

-- | Numerical operators
class IsFloating a where
    (.^.) :: (IsIntegral b a) => a -> b -> a

instance IsFloating (Exp t Float) where
    x .^. y = binop (BinopE PowF) x (fromInt y :: Exp t Float)

instance IsFloating (Exp t Double) where
    x .^. y = binop (BinopE PowF) x (fromInt y :: Exp t Double)

-- | Helpers
varE :: Var t a -> Exp t a
varE = E . VarE . unV

voidE :: Exp t ()
voidE = E UnitE

class IsIntegral a b where
    fromInt :: a -> b

-- Int32
instance Num (Exp t Int32) where
    e1 + e2 = binop (BinopE AddN) e1 e2
    e1 - e2 = binop (BinopE SubN) e1 e2
    e1 * e2 = binop (BinopE MulN) e1 e2

    negate e    = unop (UnopE NegN) e
    fromInteger = E . ConstE . Int32C . fromInteger

    abs e    = unop (UnopE AbsN) e
    signum e = unop (UnopE SignumN) e

instance Real (Exp t Int32) where
    toRational _ = error "Exp: cannot convert to rational"

instance Enum (Exp t Int32) where
    toEnum _   = error "Exp: not enumerable"
    fromEnum _ = error "Exp: not enumerable"

instance Integral (Exp t Int32) where
    quot = binop (BinopE DivN)
    rem  = binop (BinopE ModI)

    quotRem e1 e2 = (quot e1 e2, rem e1 e2)

    toInteger _ = error "Exp: cannot convert to Integer"

-- Float
instance IsIntegral (Exp t Int32) (Exp t Float) where
    fromInt = unop (UnopE (ToFloatI FloatT))

instance Num (Exp t Float) where
    e1 + e2 = binop (BinopE AddN) e1 e2
    e1 - e2 = binop (BinopE SubN) e1 e2
    e1 * e2 = binop (BinopE MulN) e1 e2

    negate e    = unop (UnopE NegN) e
    fromInteger = E . ConstE . FloatC . fromInteger

    abs e    = unop (UnopE AbsN) e
    signum e = unop (UnopE SignumN) e

instance Real (Exp t Float) where
    toRational _ = error "Exp: cannot convert to rational"

instance Fractional (Exp t Float) where
    (/) = binop (BinopE DivN)

    recip (E (UnopE RecipF e)) = E e
    recip e                    = unop (UnopE RecipF) e

    fromRational = E . ConstE . FloatC . fromRational

instance Floating (Exp t Float) where
    pi      = (E . ConstE . FloatC) pi
    exp     = unop (UnopE ExpF)
    sqrt    = unop (UnopE SqrtF)
    log     = unop (UnopE LogF)
    (**)    = binop (BinopE PowF)
    logBase = binop (BinopE LogBaseF)
    sin     = unop (UnopE SinF)
    tan     = unop (UnopE TanF)
    cos     = unop (UnopE CosF)
    asin    = unop (UnopE AsinF)
    atan    = unop (UnopE AtanF)
    acos    = unop (UnopE AcosF)
    sinh    = unop (UnopE SinhF)
    tanh    = unop (UnopE TanhF)
    cosh    = unop (UnopE CoshF)
    asinh   = unop (UnopE AsinhF)
    atanh   = unop (UnopE AtanhF)
    acosh   = unop (UnopE AcoshF)

-- Double
instance IsIntegral (Exp t Int32) (Exp t Double) where
    fromInt = unop (UnopE (ToFloatI DoubleT))

instance Num (Exp t Double) where
    e1 + e2 = binop (BinopE AddN) e1 e2
    e1 - e2 = binop (BinopE SubN) e1 e2
    e1 * e2 = binop (BinopE MulN) e1 e2

    negate e    = unop (UnopE NegN) e
    fromInteger = E . ConstE . DoubleC . fromInteger

    abs e    = unop (UnopE AbsN) e
    signum e = unop (UnopE SignumN) e

instance Real (Exp t Double) where
    toRational _ = error "Exp: cannot convert to rational"

instance Fractional (Exp t Double) where
    (/) = binop (BinopE DivN)

    recip (E (UnopE RecipF e)) = E e
    recip e                    = unop (UnopE RecipF) e

    fromRational = E . ConstE . DoubleC . fromRational

instance Floating (Exp t Double) where
    pi      = (E . ConstE . DoubleC) pi
    exp     = unop (UnopE ExpF)
    sqrt    = unop (UnopE SqrtF)
    log     = unop (UnopE LogF)
    (**)    = binop (BinopE PowF)
    logBase = binop (BinopE LogBaseF)
    sin     = unop (UnopE SinF)
    tan     = unop (UnopE TanF)
    cos     = unop (UnopE CosF)
    asin    = unop (UnopE AsinF)
    atan    = unop (UnopE AtanF)
    acos    = unop (UnopE AcosF)
    sinh    = unop (UnopE SinhF)
    tanh    = unop (UnopE TanhF)
    cosh    = unop (UnopE CoshF)
    asinh   = unop (UnopE AsinhF)
    atanh   = unop (UnopE AtanhF)
    acosh   = unop (UnopE AcoshF)

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

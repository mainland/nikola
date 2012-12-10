{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RebindableSyntax #-}
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

    -- * Equality
    IsEq(..),

    -- * Comparison operators
    IsOrd(..),

    -- * Boolean operators
    (&&*), (||*),

    -- * Bitwise operators
    IsBits(..),

    -- * Numerical operators
    IsNum(..),
    IsIntegral(..),
    (^),

    -- * Helpers
    varE, voidE,

    Lift(..),
    Unlift(..),

    Ptr,
    IsElem(..)
  ) where

import Prelude hiding ((^), fromIntegral, max, min)
import qualified Prelude as P

import Data.Bits (Bits)
import Data.Int
import Data.Typeable (Typeable)
import Data.Word

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

instance Unlift t a => IfThenElse (Exp t Bool) a where
    ifThenElse t th el =
        unlift e
      where
        th' :: Exp t (Lifted t a)
        th' = lift th

        el' :: Exp t (Lifted t a)
        el' = lift el

        e :: Exp t (Lifted t a)
        e = E $ IfThenElseE (unE t) (unE th') (unE el')

-- | A binary if-then-else combinator.
(?) :: forall t a . Unlift t a
    => Exp t Bool
    -> (a, a)
    -> a
t ? (th, el) = unlift e
  where
    th' :: Exp t (Lifted t a)
    th' = lift th

    el' :: Exp t (Lifted t a)
    el' = lift el

    e :: Exp t (Lifted t a)
    e = E $ IfThenElseE (unE t) (unE th') (unE el')

infix 4 ==*, /=*, <*, <=*, >*, >=*

-- | Embedded version of the 'Eq' class
class Eq a => IsEq a where
    (==*), (/=*) :: Exp t a -> Exp t a -> Exp t Bool
    (==*) = binop (BinopE EqO)
    (/=*) = binop (BinopE NeO)

instance IsEq Int8
instance IsEq Int16
instance IsEq Int32
instance IsEq Int64
instance IsEq Word8
instance IsEq Word16
instance IsEq Word32
instance IsEq Word64
instance IsEq Float
instance IsEq Double

-- | Embedded version of the 'Ord' class
class Ord a => IsOrd a where
    (<*), (<=*), (>*), (>=*) :: Exp t a -> Exp t a -> Exp t Bool
    (<*)  = binop (BinopE LtO)
    (<=*) = binop (BinopE LeO)
    (>*)  = binop (BinopE GtO)
    (>=*) = binop (BinopE GeO)

    max, min :: Exp t a -> Exp t a -> Exp t a
    max = binop (BinopE MaxO)
    min = binop (BinopE MinO)

instance IsOrd Int8
instance IsOrd Int16
instance IsOrd Int32
instance IsOrd Int64
instance IsOrd Word8
instance IsOrd Word16
instance IsOrd Word32
instance IsOrd Word64
instance IsOrd Float
instance IsOrd Double

-- | Embedded versions of Boolean operators
infixr 3  &&*
infixr 2  ||*

(&&*) :: Exp t Bool -> Exp t Bool -> Exp t Bool
e1 &&* e2 = binop (BinopE AndL) e1 e2

(||*) :: Exp t Bool -> Exp t Bool -> Exp t Bool
e1 ||* e2 = binop (BinopE OrL) e1 e2

-- | Embedded versions of bit-wise operators
infixl 7 &*
infixl 5 |*

class Bits a => IsBits a where
    (&*) :: Bits a => Exp t a -> Exp t a -> Exp t a
    e1 &* e2 = binop (BinopE AndB) e1 e2

    (|*) :: Bits a => Exp t a -> Exp t a -> Exp t a
    e1 |* e2 = binop (BinopE OrB) e1 e2

instance IsBits Int8
instance IsBits Int16
instance IsBits Int32
instance IsBits Int64
instance IsBits Word8
instance IsBits Word16
instance IsBits Word32
instance IsBits Word64

-- | 'IsNum' type class
class IsElem (Exp t a) => IsNum t a where
    fromInt :: Exp t Int32 -> Exp t a
    fromInt e = unop (UnopE (Cast tau)) e
      where
        tau :: ScalarType
        tau = typeOf (undefined :: Exp t a)

class (Integral (Exp t a), IsNum t a) => IsIntegral t a where
    toInt :: Exp t a -> Exp t Int32
    toInt e = unop (UnopE (Cast Int32T)) e

fromIntegral :: (IsIntegral t a, IsNum t b) => Exp t a -> Exp t b
fromIntegral = fromInt . toInt

(^) :: (Floating (Exp t a), IsNum t a, IsIntegral t b) => Exp t a -> Exp t b -> Exp t a
x ^ y = x ** fromIntegral y

-- | Helpers
varE :: Var t a -> Exp t a
varE = E . VarE . unV

voidE :: Exp t ()
voidE = E UnitE

--
-- Lifting to embedded values
--

class Lift t a where
    type Lifted t a :: *

    lift :: a -> Exp t (Lifted t a)

#define baseTypeLift(ty,con)       \
instance Lift t ty where {         \
; type Lifted t ty = ty            \
; lift = E . ConstE . con          \
} ;                                \
instance Lift t (Exp t ty) where { \
; type Lifted t (Exp t ty) = ty    \
; lift = id                        \
}

baseTypeLift(Bool,   BoolC)
baseTypeLift(Int8,   Int8C)
baseTypeLift(Int16,  Int16C)
baseTypeLift(Int32,  Int32C)
baseTypeLift(Int64,  Int64C)
baseTypeLift(Word8,  Word8C)
baseTypeLift(Word16, Word16C)
baseTypeLift(Word32, Word32C)
baseTypeLift(Word64, Word64C)
baseTypeLift(Float,  FloatC)
baseTypeLift(Double, DoubleC)

instance (Lift t a, Lift t b) => Lift t (a, b) where
    type Lifted t (a, b) = (Lifted t a, Lifted t b)

    lift (a, b) = E $ TupleE [unE ea, unE eb]
      where
        ea :: Exp t (Lifted t a)
        ea = lift a

        eb :: Exp t (Lifted t b)
        eb = lift b

--
-- Unlifting from embedded values
--

class Lift t a => Unlift t a where
    unlift ::  Exp t (Lifted t a) -> a

#define baseTypeUnlift(ty)           \
instance Unlift t (Exp t ty) where { \
; unlift = id                        \
}

baseTypeUnlift(Bool)
baseTypeUnlift(Int8)
baseTypeUnlift(Int16)
baseTypeUnlift(Int32)
baseTypeUnlift(Int64)
baseTypeUnlift(Word8)
baseTypeUnlift(Word16)
baseTypeUnlift(Word32)
baseTypeUnlift(Word64)
baseTypeUnlift(Float)
baseTypeUnlift(Double)

instance (Unlift t a, Unlift t b) => Unlift t (a, b) where
    unlift (E e) = (unlift ea, unlift eb)
      where
        ea :: Exp t (Lifted t a)
        ea = E (ProjE 0 2 e)

        eb :: Exp t (Lifted t b)
        eb = E (ProjE 1 2 e)

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

--
-- IsElem instances for tuple
--

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
         , Typeable b
         , IsElem (Exp t a)
         , IsElem (Exp t b)
         ) => IsElem (Exp t (a, b)) where
    type Rep (Exp t (a, b)) = (Rep a, Rep b)

    typeOf _ = TupleT [ typeOf (undefined :: Exp t a)
                      , typeOf (undefined :: Exp t b)
                      ]

    indexElem arr ix = E $ TupleE [unE ea , unE eb]
      where
        ea :: Exp t a
        ea = indexElem (projArr 0 2 arr) ix

        eb :: Exp t b
        eb = indexElem (projArr 1 2 arr) ix

    writeElem arr ix e = do
        writeElem (projArr 0 2 arr) ix ea
        writeElem (projArr 1 2 arr) ix eb
      where
        ea :: Exp t a
        ea = E $ ProjE 0 2 (unE e)

        eb :: Exp t b
        eb = E $ ProjE 1 2 (unE e)

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

#define isNum(ty, valcon)                           \
instance Num (Exp t ty) where                       \
{ e1 + e2     = binop (BinopE AddN) e1 e2           \
; e1 - e2     = binop (BinopE SubN) e1 e2           \
; e1 * e2     = binop (BinopE MulN) e1 e2           \
; negate e    = unop (UnopE NegN) e                 \
; fromInteger = E . ConstE . valcon . P.fromInteger \
; abs e       = unop (UnopE AbsN) e                 \
; signum e    = unop (UnopE SignumN) e              \
}

isNum(Int8,   Int8C)
isNum(Int16,  Int16C)
isNum(Int32,  Int32C)
isNum(Int64,  Int64C)
isNum(Word8,  Word8C)
isNum(Word16, Word16C)
isNum(Word32, Word32C)
isNum(Word64, Word64C)
isNum(Float,  FloatC)
isNum(Double, DoubleC)

#define isEnum(ty)                                        \
instance Enum (Exp t ty) where                            \
{ toEnum _   = error "embedded values are not enumerable" \
; fromEnum _ = error "embedded values are not enumerable" \
}

isEnum(Int8)
isEnum(Int16)
isEnum(Int32)
isEnum(Int64)
isEnum(Word8)
isEnum(Word16)
isEnum(Word32)
isEnum(Word64)

#define isReal(ty)                                                           \
instance Real (Exp t ty) where                                               \
{ toRational _ = error "an embedded value cannot be converted to a Rational" \
}

isReal(Int8)
isReal(Int16)
isReal(Int32)
isReal(Int64)
isReal(Word8)
isReal(Word16)
isReal(Word32)
isReal(Word64)
isReal(Float)
isReal(Double)

#define isIntegral(ty)                                           \
instance Integral (Exp t ty) where                               \
{ quot        = binop (BinopE QuotI)                             \
; rem         = binop (BinopE RemI)                              \
; quotRem n d = (quot n d, rem n d)                              \
; divMod  n d = if signum r ==* negate (signum d)                \
                then (q-1, r+d)                                  \
                else qr where { qr@(q,r) = quotRem n d }         \
; toInteger _ = error "cannot convert embedded value to Integer" \
}

isIntegral(Int8)
isIntegral(Int16)
isIntegral(Int32)
isIntegral(Int64)
isIntegral(Word8)
isIntegral(Word16)
isIntegral(Word32)
isIntegral(Word64)

#define isFractional(ty,valcon) \
instance Fractional (Exp t ty) where \
{ (/)                        = binop (BinopE DivF) \
; recip (E (UnopE RecipF e)) = E e \
; recip e                    = unop (UnopE RecipF) e \
; fromRational               = E . ConstE . valcon . fromRational \
}

isFractional(Float, FloatC)
isFractional(Double, DoubleC)

#define isFloating(ty,valcon)        \
instance Floating (Exp t ty) where   \
{ pi      = (E . ConstE . valcon) pi \
; exp     = unop (UnopE ExpF)        \
; sqrt    = unop (UnopE SqrtF)       \
; log     = unop (UnopE LogF)        \
; (**)    = binop (BinopE PowF)      \
; logBase = binop (BinopE LogBaseF)  \
; sin     = unop (UnopE SinF)        \
; tan     = unop (UnopE TanF)        \
; cos     = unop (UnopE CosF)        \
; asin    = unop (UnopE AsinF)       \
; atan    = unop (UnopE AtanF)       \
; acos    = unop (UnopE AcosF)       \
; sinh    = unop (UnopE SinhF)       \
; tanh    = unop (UnopE TanhF)       \
; cosh    = unop (UnopE CoshF)       \
; asinh   = unop (UnopE AsinhF)      \
; atanh   = unop (UnopE AtanhF)      \
; acosh   = unop (UnopE AcoshF)      \
}

isFloating(Float, FloatC)
isFloating(Double, DoubleC)

-- Copyright (c) 2009-2010
--         The President and Fellows of Harvard College.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions
-- are met:
-- 1. Redistributions of source code must retain the above copyright
--    notice, this list of conditions and the following disclaimer.
-- 2. Redistributions in binary form must reproduce the above copyright
--    notice, this list of conditions and the following disclaimer in the
--    documentation and/or other materials provided with the distribution.
-- 3. Neither the name of the University nor the names of its contributors
--    may be used to endorse or promote products derived from this software
--    without specific prior written permission.

-- THIS SOFTWARE IS PROVIDED BY THE UNIVERSITY AND CONTRIBUTORS ``AS IS'' AND
-- ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
-- IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
-- ARE DISCLAIMED.  IN NO EVENT SHALL THE UNIVERSITY OR CONTRIBUTORS BE LIABLE
-- FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
-- OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
-- HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
-- LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
-- OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
-- SUCH DAMAGE.

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Array.Nikola.Language.Smart (
    IsBool(..),
    IsEq(..),
    IsOrd(..),
    (.&.),
    (?),
    map,
    mapM,
    permute,
    permuteM,
    zipWith,
    zipWith3,
    zipWith3M,
    blockedScanM,
    blockedNacsM,
    blockedAddM
  ) where

import Prelude hiding (map, mapM, zipWith, zipWith3)

import Data.Int

import Data.Array.Nikola.Reify
import Data.Array.Nikola.Representable
import Data.Array.Nikola.Language.Syntax

instance Eq (Exp a) where
    _ == _ = error "Cannot compare values of type Exp a"

instance Ord (Exp a) where
    _ `compare` _ = error "Cannot compare values of type Exp a"

unop :: Unop -> Exp a -> Exp b
unop Fsqrt (E (FloatE x)) = E (FloatE (sqrt x))
unop op e                 = (E . UnopE op . unE) e

binop :: Binop -> Exp a -> Exp b -> Exp c
binop Fadd (E (FloatE x)) (E (FloatE y)) = E (FloatE (x + y))
binop Fsub (E (FloatE x)) (E (FloatE y)) = E (FloatE (x - y))
binop Fmul (E (FloatE x)) (E (FloatE y)) = E (FloatE (x * y))
binop Fdiv (E (FloatE x)) (E (FloatE y)) = E (FloatE (x / y))
binop op e1 e2                           = E $ BinopE op (unE e1) (unE e2)

class IsBool a where
    true :: a
    false :: a

class IsEq a where
    (.==.) :: a -> a -> Exp Bool
    (./=.) :: a -> a -> Exp Bool

class IsEq a => IsOrd a where
    (.>.)  :: a -> a -> Exp Bool
    (.>=.) :: a -> a -> Exp Bool
    (.<.)  :: a -> a -> Exp Bool
    (.<=.) :: a -> a -> Exp Bool

instance IsBool (Exp Bool) where
    false = E (BoolE False)
    true  = E (BoolE True)

instance IsEq (Exp a) where
    e1 .==. e2 = binop Leq e1 e2
    e1 ./=. e2 = binop Lne e1 e2

instance IsOrd (Exp a) where
    e1 .>. e2  = binop Lgt e1 e2
    e1 .>=. e2 = binop Lge e1 e2
    e1 .<. e2  = binop Llt e1 e2
    e1 .<=. e2 = binop Lle e1 e2

instance Num (Exp Int32) where
    e1 + e2 = binop Iadd e1 e2
    e1 - e2 = binop Isub e1 e2
    e1 * e2 = binop Imul e1 e2

    negate = unop Ineg
    abs    = unop Iabs
    signum = unop Isignum

    fromInteger = E . Int32E . fromInteger

instance Num (Exp Float) where
    e1 + e2 = binop Fadd e1 e2
    e1 - e2 = binop Fsub e1 e2
    e1 * e2 = binop Fmul e1 e2

    negate = unop Fneg
    abs    = unop Fabs
    signum = unop Fsignum

    fromInteger = E . FloatE . fromInteger

instance Fractional (Exp Float) where
    e1 / e2 = binop Fdiv e1 e2
    recip e = E $ BinopE Fdiv (FloatE 1.0) (unE e)

    fromRational = E . FloatE . fromRational

instance Floating (Exp Float) where
    pi            = E $ FloatE pi
    exp           = unop Fexp
    sqrt          = unop Fsqrt
    log           = unop Flog
    e1 ** e2      = binop Fpow e1 e2
    logBase e1 e2 = binop FlogBase e1 e2
    sin           = unop Fsin
    tan           = unop Ftan
    cos           = unop Fcos
    asin          = unop Fasin
    atan          = unop Fatan
    acos          = unop Facos
    sinh          = unop Fsinh
    tanh          = unop Ftanh
    cosh          = unop Fcosh
    asinh         = unop Fasinh
    atanh         = unop Fatanh
    acosh         = unop Facosh

(.&.) :: Exp Int32 -> Exp Int32 -> Exp Int32
e1 .&. e2 = binop Band e1 e2

(?) :: Exp Bool -> (Exp a, Exp a) -> Exp a
test ? (thene, elsee) = E $
    IfteE (unE test) (unE thene) (unE elsee)

map :: (Elt a, Elt b)
    => (Exp a -> Exp b)
    -> Exp (f a)
    -> Exp (g b)
map f v =
    E $ MapE (delayFun f) (unE v)

mapM :: (Elt a, Elt b)
     => (Exp a -> Exp b)
     -> Exp (Vector a)
     -> Exp (Vector b)
     -> IO (Exp ())
mapM f xs ys =
    return $ E $ MapME (delayFun f) (unE xs) (unE ys)

permute :: Exp (f a) -> Exp (g Int32) -> Exp (h a)
permute xs is =
    E $ PermuteE (unE xs) (unE is)

permuteM :: Elt a
         => Exp (Vector a)
         -> Exp (Vector Int32)
         -> Exp (Vector a)
         -> IO (Exp ())
permuteM xs is ys =
    return $ E $ PermuteME (unE xs) (unE is) (unE ys)

zipWith :: (Elt a, Elt b, Elt c)
        => (Exp a -> Exp b -> Exp c)
        -> Exp (f a)
        -> Exp (g b)
        -> Exp (h c)
zipWith f v1 v2 =
    E $ ZipWithE (delayFun f) (unE v1) (unE v2)

zipWith3 :: (Elt a, Elt b, Elt c, Elt d)
         => (Exp a -> Exp b -> Exp c -> Exp d)
         -> Exp (f a)
         -> Exp (g b)
         -> Exp (h c)
         -> Exp (i d)
zipWith3 f v1 v2 v3 =
    E $ ZipWith3E (delayFun f) (unE v1) (unE v2) (unE v3)

zipWith3M :: (Elt a, Elt b, Elt c, Elt d)
          => (Exp a -> Exp b -> Exp c -> Exp d)
          -> Exp (Vector a)
          -> Exp (Vector b)
          -> Exp (Vector c)
          -> Exp (Vector d)
          -> IO (Exp ())
zipWith3M f xs ys zs results =
    return $ E $ ZipWith3ME (delayFun f) (unE xs) (unE ys) (unE zs) (unE results)

blockedScanM :: (Elt a)
             => (Exp a -> Exp a -> Exp a)
             -> Exp a
             -> Exp (Vector a)
             -> IO (Exp (Vector a))
blockedScanM f z xs =
    return $ E $ BlockedScanME (delayFun f) (unE z) (unE xs)

blockedNacsM :: (Elt a)
             => (Exp a -> Exp a -> Exp a)
             -> Exp a
             -> Exp (Vector a)
             -> IO (Exp (Vector a))
blockedNacsM f z xs =
    return $ E $ BlockedNacsME (delayFun f) (unE z) (unE xs)

blockedAddM :: (Elt a)
            => Exp (Vector a)
            -> Exp (Vector a)
            -> IO (Exp ())
blockedAddM xs sums =
    return $ E $ BlockedAddME (unE xs) (unE sums)

delayFun :: ReifiableFun a b
         => (a -> b)
         -> DExp
delayFun = DelayedE . reifyLam

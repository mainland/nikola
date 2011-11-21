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

module Nikola.Smart (
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

import Nikola.Embeddable
import Nikola.Reify
import Nikola.Syntax

instance Eq (Exp a) where
    _ == _ = error "Cannot compare values of type Exp a"

instance Ord (Exp a) where
    _ `compare` _ = error "Cannot compare values of type Exp a"

unop :: Unop -> Exp a -> Exp b
unop op = E . UnopE op . unE

binop :: Binop -> Exp a -> Exp b -> Exp c
binop op e1 e2 = E $ BinopE op (unE e1) (unE e2)

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

instance Num (Exp Int) where
    e1 + e2 = binop Iadd e1 e2
    e1 - e2 = binop Isub e1 e2
    e1 * e2 = binop Imul e1 e2

    negate = unop Ineg
    abs    = unop Iabs
    signum = unop Isignum

    fromInteger = E . IntE . fromInteger

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

(.&.) :: Exp Int -> Exp Int -> Exp Int
e1 .&. e2 = binop Band e1 e2

(?) :: Exp Bool -> (Exp a, Exp a) -> Exp a
test ? (thene, elsee) = E $
    IfteE (unE test) (unE thene) (unE elsee)

map :: (IsVector f a, IsVector g b)
    => (Exp a -> Exp b) -> Exp (f a) -> Exp (g b)
map f v =
    E $ MapE (delayFun f) (unE v)

mapM :: (Embeddable a, Embeddable b)
     => (Exp a -> Exp b)
     -> Exp (CUVector a)
     -> Exp (CUVector b)
     -> IO (Exp ())
mapM f xs ys =
    return $ E $ MapME (delayFun f) (unE xs) (unE ys)

permute :: (IsVector f a, IsVector g Int, IsVector h a)
        => Exp (f a) -> Exp (g Int) -> Exp (h a)
permute xs is =
    E $ PermuteE (unE xs) (unE is)

permuteM :: Exp (CUVector a)
         -> Exp (CUVector Int)
         -> Exp (CUVector a)
         -> IO (Exp ())
permuteM xs is ys =
    return $ E $ PermuteME (unE xs) (unE is) (unE ys)

zipWith :: (IsVector f a,
            IsVector g b,
            IsVector h c)
        => (Exp a -> Exp b -> Exp c) -> Exp (f a) -> Exp (g b) -> Exp (h c)
zipWith f v1 v2 =
    E $ ZipWithE (delayFun f) (unE v1) (unE v2)

zipWith3 :: (IsVector f a,
             IsVector g b,
             IsVector h c,
             IsVector i d)
         => (Exp a -> Exp b -> Exp c -> Exp d)
         -> Exp (f a)
         -> Exp (g b)
         -> Exp (h c)
         -> Exp (i d)
zipWith3 f v1 v2 v3 =
    E $ ZipWith3E (delayFun f) (unE v1) (unE v2) (unE v3)

zipWith3M :: (IsScalar a, IsScalar b, IsScalar c, IsScalar d)
          => (Exp a -> Exp b -> Exp c -> Exp d)
          -> Exp (CUVector a)
          -> Exp (CUVector b)
          -> Exp (CUVector c)
          -> Exp (CUVector d)
          -> IO (Exp ())
zipWith3M f xs ys zs results =
    return $ E $ ZipWith3ME (delayFun f) (unE xs) (unE ys) (unE zs) (unE results)

blockedScanM :: (IsScalar a)
             => (Exp a -> Exp a -> Exp a)
             -> Exp a
             -> Exp (CUVector a)
             -> IO (Exp (CUVector a))
blockedScanM f z xs =
    return $ E $ BlockedScanME (delayFun f) (unE z) (unE xs)

blockedNacsM :: (IsScalar a)
             => (Exp a -> Exp a -> Exp a)
             -> Exp a
             -> Exp (CUVector a)
             -> IO (Exp (CUVector a))
blockedNacsM f z xs =
    return $ E $ BlockedNacsME (delayFun f) (unE z) (unE xs)

blockedAddM :: (IsScalar a)
            => Exp (CUVector a)
            -> Exp (CUVector a)
            -> IO (Exp ())
blockedAddM xs sums =
    return $ E $ BlockedAddME (unE xs) (unE sums)

delayFun :: ReifiableFun a b
         => (a -> b)
         -> DExp
delayFun = DelayedE . reifyfun

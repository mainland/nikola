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
{-# LANGUAGE MultiParamTypeClasses #-}

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

import Data.Array.Nikola.Embed
import Data.Array.Nikola.Language.Syntax

unop :: Unop -> Exp t a -> Exp t b
unop Fsqrt (E (FloatE x)) = E (FloatE (sqrt x))
unop op e                 = (E . UnopE op . unE) e

binop :: Binop -> Exp t a -> Exp t b -> Exp t c
binop Fadd (E (FloatE x)) (E (FloatE y)) = E (FloatE (x + y))
binop Fsub (E (FloatE x)) (E (FloatE y)) = E (FloatE (x - y))
binop Fmul (E (FloatE x)) (E (FloatE y)) = E (FloatE (x * y))
binop Fdiv (E (FloatE x)) (E (FloatE y)) = E (FloatE (x / y))
binop op e1 e2                           = E $ BinopE op (unE e1) (unE e2)

class IsBool a where
    true  :: a
    false :: a

class IsEq t a where
    (.==.) :: a -> a -> Exp t Bool
    (./=.) :: a -> a -> Exp t Bool

class IsEq t a => IsOrd t a where
    (.>.)  :: a -> a -> Exp t Bool
    (.>=.) :: a -> a -> Exp t Bool
    (.<.)  :: a -> a -> Exp t Bool
    (.<=.) :: a -> a -> Exp t Bool

instance IsBool (Exp t Bool) where
    false = E (BoolE False)
    true  = E (BoolE True)

instance IsEq t (Exp t a) where
    e1 .==. e2 = binop Leq e1 e2
    e1 ./=. e2 = binop Lne e1 e2

instance IsOrd t (Exp t a) where
    e1 .>. e2  = binop Lgt e1 e2
    e1 .>=. e2 = binop Lge e1 e2
    e1 .<. e2  = binop Llt e1 e2
    e1 .<=. e2 = binop Lle e1 e2

instance Num (Exp t Int32) where
    e1 + e2 = binop Iadd e1 e2
    e1 - e2 = binop Isub e1 e2
    e1 * e2 = binop Imul e1 e2

    negate = unop Ineg
    abs    = unop Iabs
    signum = unop Isignum

    fromInteger = E . Int32E . fromInteger

instance Num (Exp t Float) where
    e1 + e2 = binop Fadd e1 e2
    e1 - e2 = binop Fsub e1 e2
    e1 * e2 = binop Fmul e1 e2

    negate = unop Fneg
    abs    = unop Fabs
    signum = unop Fsignum

    fromInteger = E . FloatE . fromInteger

instance Fractional (Exp t Float) where
    e1 / e2 = binop Fdiv e1 e2
    recip e = E $ BinopE Fdiv (FloatE 1.0) (unE e)

    fromRational = E . FloatE . fromRational

instance Floating (Exp t Float) where
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

(.&.) :: Exp t Int32 -> Exp t Int32 -> Exp t Int32
e1 .&. e2 = binop Band e1 e2

(?) :: Exp t Bool -> (Exp t a, Exp t a) -> Exp t a
test ? (thene, elsee) = E $
    IfteE (unE test) (unE thene) (unE elsee)

map :: (Elt t a, Elt t b, IsVector t v1 a, IsVector t v2 a)
    => (Exp t a -> Exp t b)
    -> Exp t (v1 a)
    -> Exp t (v2 b)
map f v =
    E $ MapE (delayFun f) (unE v)

mapM :: (Elt t a, Elt t b, IsVector t v a)
     => (Exp t a -> Exp t b)
     -> Exp t (v a)
     -> Exp t (v b)
     -> IO (Exp t ())
mapM f xs ys =
    return $ E $ MapME (delayFun f) (unE xs) (unE ys)

permute :: Exp t (f a) -> Exp t (g Int32) -> Exp t (h a)
permute xs is =
    E $ PermuteE (unE xs) (unE is)

permuteM :: (Elt t a, IsVector t v a)
         => Exp t (v a)
         -> Exp t (v Int32)
         -> Exp t (v a)
         -> IO (Exp t ())
permuteM xs is ys =
    return $ E $ PermuteME (unE xs) (unE is) (unE ys)

zipWith :: (Elt t a, Elt t b, Elt t c)
        => (Exp t a -> Exp t b -> Exp t c)
        -> Exp t (f a)
        -> Exp t (g b)
        -> Exp t (h c)
zipWith f v1 v2 =
    E $ ZipWithE (delayFun f) (unE v1) (unE v2)

zipWith3 :: (Elt t a, Elt t b, Elt t c, Elt t d)
         => (Exp t a -> Exp t b -> Exp t c -> Exp t d)
         -> Exp t (f a)
         -> Exp t (g b)
         -> Exp t (h c)
         -> Exp t (i d)
zipWith3 f v1 v2 v3 =
    E $ ZipWith3E (delayFun f) (unE v1) (unE v2) (unE v3)

zipWith3M :: (Elt t a, Elt t b, Elt t c, Elt t d, IsVector t v a)
          => (Exp t a -> Exp t b -> Exp t c -> Exp t d)
          -> Exp t (v a)
          -> Exp t (v b)
          -> Exp t (v c)
          -> Exp t (v d)
          -> IO (Exp t ())
zipWith3M f xs ys zs results =
    return $ E $ ZipWith3ME (delayFun f) (unE xs) (unE ys) (unE zs) (unE results)

blockedScanM :: (Elt t a, IsVector t v a)
             => (Exp t a -> Exp t a -> Exp t a)
             -> Exp t a
             -> Exp t (v a)
             -> IO (Exp t (v a))
blockedScanM f z xs =
    return $ E $ BlockedScanME (delayFun f) (unE z) (unE xs)

blockedNacsM :: (Elt t a, IsVector t v a)
             => (Exp t a -> Exp t a -> Exp t a)
             -> Exp t a
             -> Exp t (v a)
             -> IO (Exp t (v a))
blockedNacsM f z xs =
    return $ E $ BlockedNacsME (delayFun f) (unE z) (unE xs)

blockedAddM :: (Elt t a, IsVector t v a)
            => Exp t (v a)
            -> Exp t (v a)
            -> IO (Exp t ())
blockedAddM xs sums =
    return $ E $ BlockedAddME (unE xs) (unE sums)

delayFun :: ReifiableFun a b
         => (a -> b)
         -> DExp
delayFun = DelayedE . reifyLam

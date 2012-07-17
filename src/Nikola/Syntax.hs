-- Copyright (c) 2009-2012
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
--
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

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Nikola.Syntax (
    ParamIdx(..),
    N(..),
    ExecConfig(..),
    nGridDimX,
    nGridDimY,
    threadBlockWidth,
    maxGridWidth,

    Tau(..),
    Rho(..),

    Var(..),
    Unop(..),
    Binop(..),
    DExp(..),

    Exp(..),

    unScalarT,
    vectorT,
    matrixT,
    freeVars
) where

import Data.Generics (Data, Typeable)
import qualified Data.Set as Set
import Text.PrettyPrint.Mainland

import Nikola.Pretty
import {-# SOURCE #-} Nikola.Reify.Monad

-- |Function parameter index.
newtype ParamIdx = ParamIdx { unParamIdx :: Int }
  deriving (Eq, Ord, Data, Typeable)

instance Pretty ParamIdx where
    ppr pi = text "#" <> ppr (unParamIdx pi)

-- |Gross constant...but we need it to perform type-level calculations.
threadBlockWidth :: Integer
threadBlockWidth = 128

maxGridWidth :: Integer
maxGridWidth = 32768

-- |Type-level "numbers". These numbers are all derived from the size of a
-- function arguments.
data N = NVecLength ParamIdx
       | NMatStride ParamIdx
       | NMatRows ParamIdx
       | NMatCols ParamIdx

       | N Int
       | NAdd N N
       | NSub N N
       | NMul N N
       | NNegate N

       | NDiv N N
       | NMod N N

       | NMin [N]
       | NMax [N]
  deriving (Eq, Ord, Data, Typeable)

instance Num N where
    (+) = NAdd
    (-) = NSub
    (*) = NMul
    negate = NNegate
    fromInteger = N . fromInteger

    abs = error "abs not implemented for N"
    signum = error "signum not implemented for N"

instance Real N where
    toRational = error "toRational not implemented for N"

instance Enum N where
    toEnum = N
    fromEnum = error "fromEnum not implemented for N"

instance Integral N where
    div = NDiv
    mod = NMod

    quot = error "quot not implemented for N"
    rem = error "rem not implemented for N"
    quotRem = error "quotRem not implemented for N"
    toInteger = error "toInteger not implemented for N"

-- | An execution configuration.
data ExecConfig = ExecConfig
    {  gridDimX  :: N
    ,  gridDimY  :: N
    ,  blockDimX :: Int
    ,  blockDimY :: Int
    ,  blockDimZ :: Int
    }
  deriving (Show, Data, Typeable)

nGridDimX :: Integral a
          => N
          -> a
          -> N
nGridDimX n w_ =
    NMin [NMax [1, (n + w - 1) `div` w], fromIntegral maxGridWidth]
  where
    w :: N
    w = fromIntegral w_

nGridDimY :: Integral a
          => N
          -> a
          -> N
nGridDimY n w_ =
    NMax [1, (n + (fromIntegral maxGridWidth)*w - 1) `div` ((fromIntegral maxGridWidth)*w)]
  where
    w :: N
    w = fromIntegral w_

data Tau = UnitT
         | BoolT
         | IntT
         | FloatT
  deriving (Eq, Ord, Data, Typeable)

data Rho = ScalarT Tau
         | VectorT Tau N
         | MatrixT Tau N N N
         | FunT [Rho] Rho
  deriving (Eq, Ord, Data, Typeable)

unScalarT :: Rho -> Tau
unScalarT (ScalarT tau) = tau
unScalarT rho           = error $ "Not a scalar type: " ++ show rho

vectorT :: Tau -> ParamIdx -> Rho
vectorT tau n = VectorT tau (NVecLength n)

matrixT :: Tau -> ParamIdx -> ParamIdx -> ParamIdx -> Rho
matrixT tau s r c = MatrixT tau (NMatStride s) (NMatRows r) (NMatCols c)

data Var = Var String
  deriving (Eq, Ord, Typeable)

data Unop = Lnot

          | Ineg
          | Iabs

          | Isignum
          | Fneg
          | Fabs
          | Fsignum

          | Fexp
          | Fsqrt
          | Flog
          | Fsin
          | Ftan
          | Fcos
          | Fasin
          | Fatan
          | Facos
          | Fsinh
          | Ftanh
          | Fcosh
          | Fasinh
          | Fatanh
          | Facosh

data Binop = Land
           | Lor

           | Leq
           | Lne
           | Lgt
           | Lge
           | Llt
           | Lle

           | Band

           | Iadd
           | Isub
           | Imul
           | Idiv

           | Fadd
           | Fsub
           | Fmul
           | Fdiv

           | Fpow
           | FlogBase

data DExp = VarE Var
          | DelayedE (R DExp)
          | LetE Var Rho DExp DExp
          | LamE [(Var, Rho)] DExp
          | AppE DExp [DExp]
          | BoolE Bool
          | IntE Int
          | FloatE Double
          | UnopE Unop DExp
          | BinopE Binop DExp DExp
          | IfteE DExp DExp DExp
          | MapE DExp DExp
          | MapME DExp DExp DExp
          | PermuteE DExp DExp
          | PermuteME DExp DExp DExp
          | ZipWithE DExp DExp DExp
          | ZipWith3E DExp DExp DExp DExp
          | ZipWith3ME DExp DExp DExp DExp DExp
          | ScanE DExp DExp DExp
          | BlockedScanME DExp DExp DExp
          | BlockedNacsME DExp DExp DExp
          | BlockedAddME DExp DExp
  deriving (Typeable)

-- | A wrapping of the core 'DExp' type that provides a phantom type parameter.
newtype Exp a = E { unE :: DExp }
  deriving (Show, Typeable)

freeVars :: DExp -> Set.Set Var
freeVars (VarE v)                   = Set.singleton v
freeVars (DelayedE _)               = error "freeVars: encountered DelayedE"
freeVars (LetE v _ e1 e2)           = freeVars e1
                                      `Set.union`
                                      (Set.delete v (freeVars e2))
freeVars (LamE _ _)                 = Set.empty
freeVars (AppE f es)                = foldr Set.union (freeVars f)
                                      (map freeVars es)
freeVars (BoolE _)                  = Set.empty
freeVars (IntE _)                   = Set.empty
freeVars (FloatE _)                 = Set.empty
freeVars (UnopE _ e)                = freeVars e
freeVars (BinopE _ e1 e2)           = freeVars e1
                                      `Set.union`
                                      freeVars e2
freeVars (IfteE e1 e2 e3)           = freeVars e1
                                      `Set.union`
                                      freeVars e2
                                      `Set.union`
                                      freeVars e3
freeVars (MapE _ e)                 = freeVars e
freeVars (MapME _ v1 v2)            = freeVars v1
                                      `Set.union`
                                      freeVars v2
freeVars (PermuteE v1 v2)           = freeVars v1
                                      `Set.union`
                                      freeVars v2
freeVars (PermuteME v1 v2 v3)       = freeVars v1
                                      `Set.union`
                                      freeVars v2
                                      `Set.union`
                                      freeVars v3
freeVars (ZipWithE _ e1 e2)         = freeVars e1
                                      `Set.union`
                                      freeVars e2
freeVars (ZipWith3E _ e1 e2 e3)     = freeVars e1
                                      `Set.union`
                                      freeVars e2
                                      `Set.union`
                                      freeVars e3
freeVars (ZipWith3ME _ e1 e2 e3 e4) = freeVars e1
                                      `Set.union`
                                       freeVars e2
                                       `Set.union`
                                       freeVars e3
                                       `Set.union`
                                       freeVars e4
freeVars (ScanE _ e1 e2)            = freeVars e1
                                      `Set.union`
                                      freeVars e2
freeVars (BlockedScanME _ z xs)     = freeVars z
                                      `Set.union`
                                      freeVars xs
freeVars (BlockedNacsME _ z xs)     = freeVars z
                                      `Set.union`
                                      freeVars xs
freeVars (BlockedAddME xs sums)     = freeVars xs
                                      `Set.union`
                                      freeVars sums

landPrec :: Int
landPrec = 3

eqPrec :: Int
eqPrec = 4

lorPrec :: Int
lorPrec = 2

bandPrec :: Int
bandPrec = 7

powPrec :: Int
powPrec = 8

instance Pretty N where
    pprPrec _ (NVecLength n) = text "veclen" <+> parens (ppr n)
    pprPrec _ (NMatStride n) = text "matstride" <+> parens (ppr n)
    pprPrec _ (NMatRows n)   = text "matrows" <+> parens (ppr n)
    pprPrec _ (NMatCols n)   = text "matcols" <+> parens (ppr n)

    pprPrec _ (N i) = ppr i

    pprPrec p (NAdd n1 n2) =
        infixop p (infixl_ addPrec) (text "+") n1 n2

    pprPrec p (NSub n1 n2) =
        infixop p (infixl_ addPrec) (text "-") n1 n2

    pprPrec p (NMul n1 n2) =
        infixop p (infixl_ mulPrec) (text "*") n1 n2

    pprPrec p (NNegate n) =
        parensIf (p > addPrec) $
        text "-" <> pprPrec addPrec n

    pprPrec p (NDiv n1 n2) =
        infixop p defaultFixity (text "`div`") n1 n2

    pprPrec p (NMod n1 n2) =
        infixop p defaultFixity (text "`mod`") n1 n2

    pprPrec _ (NMin ns) =
        text "min" <> parens (commasep (map ppr ns))

    pprPrec _ (NMax ns) =
        text "max" <> parens (commasep (map ppr ns))

instance Show N where
    show = show . ppr

instance Pretty Tau where
    ppr UnitT  = text "Unit"
    ppr BoolT  = text "Bool"
    ppr IntT   = text "Int"
    ppr FloatT = text "Float"

instance Show Tau where
    show = show . ppr

instance Pretty Rho where
    pprPrec _ (ScalarT tau) =
        ppr tau

    pprPrec _ (VectorT tau n) =
        ppr tau <+> brackets (ppr n)

    pprPrec _ (MatrixT tau s r c) =
        ppr tau <+> brackets (commasep [ppr s, ppr r, ppr c])

    pprPrec p (FunT tau1 tau2) =
        parensIf (p > appPrec) $
        infixop 0 (infixr_ 0) (text "->") tau1 tau2

instance Show Rho where
    show = show . ppr

instance Pretty Var where
    ppr (Var v) = text v

instance Show Var where
    show = show . ppr

instance Pretty DExp where
    pprPrec _ (VarE v) =
        ppr v

    pprPrec _ (DelayedE _) =
        text "Delayed"

    pprPrec p (LetE v tau e1 e2) =
        parensIf (p > appPrec) $
        nest 4 (text "let" <+>
            embrace [ppr v <+> text "::" <+> ppr tau <+>
                     text "=" <+> ppr e1]) </>
        text "in" </>
        ppr e2

    pprPrec p (LamE vtaus e) =
        parensIf (p > appPrec) $
        text "\\" <+> spread (map pp vtaus) <+> text "->" <+> ppr e
      where
        pp :: (Var, Rho) -> Doc
        pp (v, tau) =
            parens $
            ppr v <+> text "::" <+> ppr tau

    pprPrec p (AppE f es) =
        parensIf (p > appPrec) $
        folddoc (<+/>) (map (pprPrec appPrec1) (f : es))

    pprPrec _ (BoolE n) =
        (text . show) n

    pprPrec _ (IntE n) =
        (text . show) n

    pprPrec _ (FloatE n) =
        (text . show) n

    pprPrec p (UnopE op e) =
        parensIf (p > prec op) $
        ppr op  <+> pprPrec (prec op + 1) e
      where
        prec :: Unop -> Int
        prec Ineg = addPrec
        prec Fneg = addPrec
        prec _    = appPrec

    pprPrec p (BinopE FlogBase e1 e2) =
        parensIf (p > appPrec) $
        text "logBase" <+>
        pprPrec appPrec1 e1 <+>
        pprPrec appPrec1 e2

    pprPrec p (BinopE op e1 e2) =
        infixop p (fixity op) (ppr op) e1 e2
      where
        fixity :: Binop -> Fixity
        fixity Land = infixr_ landPrec
        fixity Lor  = infixr_ lorPrec

        fixity Leq = infix_ eqPrec
        fixity Lne = infix_ eqPrec
        fixity Lgt = infix_ eqPrec
        fixity Lge = infix_ eqPrec
        fixity Llt = infix_ eqPrec
        fixity Lle = infix_ eqPrec

        fixity Band = infixl_ bandPrec

        fixity Iadd = infixl_ addPrec
        fixity Isub = infixl_ addPrec
        fixity Imul = infixl_ mulPrec
        fixity Idiv = infixl_ mulPrec

        fixity Fadd = infixl_ addPrec
        fixity Fsub = infixl_ addPrec
        fixity Fmul = infixl_ mulPrec
        fixity Fdiv = infixl_ mulPrec

        fixity Fpow     = infixr_ powPrec
        fixity FlogBase = infixl_ mulPrec

    pprPrec p (IfteE teste thene elsee) =
        parensIf (p > appPrec) $
        text "if" <+> ppr teste
        <+/> text "then" <+> ppr thene
        <+/> text "else" <+> ppr elsee

    pprPrec p (MapE f e) =
        parensIf (p > appPrec) $ nest 4 $
        text "map" <+> pprPrec appPrec1 f <+> pprPrec appPrec1 e

    pprPrec p (MapME f e1 e2) =
        parensIf (p > appPrec) $ nest 4 $
        text "mapM" <+> pprPrec appPrec1 f <+>
           pprPrec appPrec1 e1 <+> pprPrec appPrec1 e2

    pprPrec p (PermuteE e1 e2) =
        parensIf (p > appPrec) $ nest 4 $
        text "permute" <+> pprPrec appPrec1 e1 <+> pprPrec appPrec1 e2

    pprPrec p (PermuteME e2 e1 e3) =
        parensIf (p > appPrec) $ nest 4 $
        text "permuteM" <+> pprPrec appPrec1 e1 <+>
            pprPrec appPrec1 e2 <+>
            pprPrec appPrec1 e3

    pprPrec p (ZipWithE f e1 e2) =
        parensIf (p > appPrec) $ nest 4 $
        text "zipWith" <+>
            pprPrec appPrec1 f <+>
            pprPrec appPrec1 e1 <+>
            pprPrec appPrec1 e2

    pprPrec p (ZipWith3E f e1 e2 e3) =
        parensIf (p > appPrec) $ nest 4 $
        text "zipWith3" <+>
            pprPrec appPrec1 f <+>
            pprPrec appPrec1 e1 <+>
            pprPrec appPrec1 e2 <+>
            pprPrec appPrec1 e3

    pprPrec p (ZipWith3ME f e1 e2 e3 e4) =
        parensIf (p > appPrec) $ nest 4 $
        text "zipWithM3" <+>
            pprPrec appPrec1 f <+>
            pprPrec appPrec1 e1 <+>
            pprPrec appPrec1 e2 <+>
            pprPrec appPrec1 e3 <+>
            pprPrec appPrec1 e4

    pprPrec p (ScanE f e1 e2) =
        parensIf (p > appPrec) $ nest 4 $
        text "scan" <+>
            pprPrec appPrec1 f <+>
            pprPrec appPrec1 e1 <+>
            pprPrec appPrec1 e2

    pprPrec p (BlockedScanME f z xs) =
        parensIf (p > appPrec) $ nest 4 $
        text "blockedScanM" <+>
            pprPrec appPrec1 f <+>
            pprPrec appPrec1 z <+>
            pprPrec appPrec1 xs

    pprPrec p (BlockedNacsME f z xs) =
        parensIf (p > appPrec) $ nest 4 $
        text "blockedNacsME" <+>
            pprPrec appPrec1 f <+>
            pprPrec appPrec1 z <+>
            pprPrec appPrec1 xs

    pprPrec p (BlockedAddME xs sums) =
        parensIf (p > appPrec) $ nest 4 $
        text "blockedAddM" <+>
            pprPrec appPrec1 xs <+>
            pprPrec appPrec1 sums

instance Show DExp where
    show = show . ppr

instance Pretty Unop where
    ppr Lnot    = text "not"

    ppr Ineg    = text "-"
    ppr Iabs    = text "abs"
    ppr Isignum = text "signum"

    ppr Fneg    = text "-"
    ppr Fabs    = text "abs"
    ppr Fsignum = text "signum"

    ppr Fexp   = text "exp"
    ppr Fsqrt  = text "sqrt"
    ppr Flog   = text "log"
    ppr Fsin   = text "sin"
    ppr Ftan   = text "tan"
    ppr Fcos   = text "cos"
    ppr Fasin  = text "asin"
    ppr Fatan  = text "atan"
    ppr Facos  = text "acos"
    ppr Fsinh  = text "sinh"
    ppr Ftanh  = text "tanh"
    ppr Fcosh  = text "cosh"
    ppr Fasinh = text "asinsh"
    ppr Fatanh = text "atanh"
    ppr Facosh = text "acosh"

instance Show Unop where
    show = show . ppr

instance Pretty Binop where
    ppr Land = text ".&&."
    ppr Lor  = text ".||."

    ppr Leq  = text ".==."
    ppr Lne  = text "./=."

    ppr Lgt = text ".>."
    ppr Lge = text ".>=."
    ppr Llt = text ".<."
    ppr Lle = text ".<=."

    ppr Band = text ".&."

    ppr Iadd = text "+"
    ppr Isub = text "-"
    ppr Imul = text "*"
    ppr Idiv = text "/"

    ppr Fadd = text "+"
    ppr Fsub = text "-"
    ppr Fmul = text "*"
    ppr Fdiv = text "/"

    ppr Fpow     = text "**"
    ppr FlogBase = text "`logBase`"

instance Show Binop where
    show = show . ppr

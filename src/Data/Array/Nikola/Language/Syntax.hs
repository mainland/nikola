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
-- Module      : Data.Array.Nikola.Language.Syntax
-- Copyright   : (c) The President and Fellows of Harvard College 2009-2010
-- Copyright   : (c) Geoffrey Mainland 2012
-- License     : BSD-style
--
-- Maintainer  : Geoffrey Mainland <mainland@apeiron.net>
-- Stability   : experimental
-- Portability : non-portable

module Data.Array.Nikola.Language.Syntax (
    Const(..),
    Unop(..),
    Binop(..),

    Occ(..),
    occJoin,
    occMeet,

    Var(..),
    unVar,

    ScalarType(..),
    PtrType(..),
    Type(..),
    unitT,
    boolT,
    ixScalarT,
    ixT,
    isScalarT,
    isArrayT,
    isFunT,
    flattenT,

    Exp(..),

    ProgH(..),
    ProcH(..),
    ProgK(..),
    ProcK(..),

    seqK,
    parK,
    bindK,
    syncK,

    seqH,
    bindH
) where

import Data.Generics (Data, Typeable)
import Data.Int
import Data.Monoid
import Data.Word
import Text.PrettyPrint.Mainland

import {-# SOURCE #-} Data.Array.Nikola.Language.Monad
import Data.Array.Nikola.Util.Pretty

-- | Constants.
data Const = BoolC   Bool
           | Int8C   Int8
           | Int16C  Int16
           | Int32C  Int32
           | Int64C  Int64
           | Word8C  Word8
           | Word16C Word16
           | Word32C Word32
           | Word64C Word64
           | FloatC  Float
           | DoubleC Double
  deriving (Eq, Ord, Data, Typeable)

-- | Unary operators
data Unop = Lnot

          | Ineg
          | Iabs
          | Isignum

          | Itof
          | Itod

          | Fneg
          | Fabs
          | Fsignum
          | Frecip

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

          | Dneg
          | Dabs
          | Dsignum
          | Drecip

          | Dexp
          | Dsqrt
          | Dlog
          | Dsin
          | Dtan
          | Dcos
          | Dasin
          | Datan
          | Dacos
          | Dsinh
          | Dtanh
          | Dcosh
          | Dasinh
          | Datanh
          | Dacosh
  deriving (Eq, Ord, Data, Typeable)

-- | Binary operators
data Binop = Land
           | Lor

           | Leq
           | Lne
           | Lgt
           | Lge
           | Llt
           | Lle

           | Band
           | Bor

           | Bmax
           | Bmin

           | Iadd
           | Isub
           | Imul
           | Idiv
           | Imod

           | Fadd
           | Fsub
           | Fmul
           | Fdiv

           | Fpow
           | FlogBase

           | Dadd
           | Dsub
           | Dmul
           | Ddiv

           | Dpow
           | DlogBase
  deriving (Eq, Ord, Data, Typeable)

-- | Variables
newtype Var = Var String
  deriving (Eq, Ord, Data, Typeable)

unVar :: Var -> String
unVar (Var s) = s

-- | Expression/program types.
data ScalarType = UnitT
                | BoolT
                | Int8T
                | Int16T
                | Int32T
                | Int64T
                | Word8T
                | Word16T
                | Word32T
                | Word64T
                | FloatT
                | DoubleT
                | TupleT [ScalarType]
  deriving (Eq, Ord, Data, Typeable)

newtype PtrType = PtrT ScalarType
  deriving (Eq, Ord, Data, Typeable)

data Type = ScalarT ScalarType
          | ArrayT ScalarType Int
          | FunT [Type] Type
          | MT Type
  deriving (Eq, Ord, Data, Typeable)

unitT :: Type
unitT = ScalarT UnitT

boolT :: Type
boolT = ScalarT BoolT

ixScalarT :: ScalarType
ixScalarT = Int32T

ixT :: Type
ixT = ScalarT Int32T

isScalarT :: Type -> Bool
isScalarT (ScalarT {}) = True
isScalarT _            = False

isArrayT :: Type -> Bool
isArrayT (ArrayT {}) = True
isArrayT _           = False

isFunT :: Type -> Bool
isFunT (FunT {}) = True
isFunT _         = False

flattenT :: Type -> [Type]
flattenT (ScalarT (TupleT taus)) =
    concatMap flattenT (map ScalarT taus)

flattenT tau =
    [tau]

-- Occurence information
data Occ = Never
         | Once
         | Many
  deriving (Eq, Ord, Show)

instance Pretty Occ where
    ppr = text . show

occJoin :: Occ -> Occ -> Occ
occJoin Never occ  = occ
occJoin Once  Never = Once
occJoin Once  _     = Many
occJoin Many  _     = Many

occMeet :: Occ -> Occ -> Occ
occMeet Never occ   = occ
occMeet Once  Never = Once
occMeet Once  Once  = Many
occMeet Once  Many  = Many
occMeet Many  _     = Many

-- | Expressions. These can be evaluated either on the host or the device.
data Exp = VarE Var
         | ConstE Const
         | UnitE
         | TupleE [Exp]
         | ProjE Int Int Exp
         | ProjArrE Int Int Exp
         | DimE Int Int Exp
         | LetE Var Type Occ Exp Exp
         | LamE [(Var, Type)] Exp
         | AppE Exp [Exp]
         | UnopE Unop Exp
         | BinopE Binop Exp Exp
         | IfThenElseE Exp Exp Exp
         | SwitchE Exp [(Int, Exp)] (Maybe Exp)
         | IndexE Exp Exp
         | DelayedE (R Exp Exp)
  deriving (Eq, Ord, Typeable)

instance Eq (R Exp Exp) where
    _ == _ = error "R Exp Exp: incomparable"

instance Ord (R Exp Exp) where
    compare _  _ = error "R Exp Exp: incomparable"

instance Num Exp where
    e1 + e2 = BinopE Iadd e1 e2
    e1 - e2 = BinopE Isub e1 e2
    e1 * e2 = BinopE Imul e1 e2

    negate e    = UnopE Ineg e
    fromInteger = ConstE . Int32C . fromInteger

    abs e    = UnopE Iabs e
    signum e = UnopE Isignum e

-- | Host programs. These are monadic.
data ProgH = ReturnH Exp
           | SeqH ProgH ProgH
           | LetH Var Type Exp ProgH
           | BindH Var Type ProgH ProgH
           | LiftH ProcK [Exp]
           | IfThenElseH Exp ProgH ProgH
           | AllocH Type [Exp]
           | DelayedH (R ProgH ProgH)
  deriving (Typeable)

-- | Host Procedures. The body of a procedure (in contrast to a function) is
-- monadic.
data ProcH = ProcH [(Var, Type)] ProgH
  deriving (Typeable)

-- | Kernel programs. These are monadic.
data ProgK = ReturnK Exp
           | SeqK ProgK ProgK
           | ParK ProgK ProgK
           | LetK Var Type Exp ProgK
           | BindK Var Type ProgK ProgK
           | ForK [Var] [Exp] ProgK
           | ParforK [Var] [Exp] ProgK
           | IfThenElseK Exp ProgK ProgK
           | WriteK Exp Exp Exp
           | SyncK
           | DelayedK (R ProgK ProgK)
  deriving (Typeable)

-- | Kernel Procedures. The body of a procedure (in contrast to a function) is
-- monadic.
data ProcK = ProcK [(Var, Type)] ProgK
  deriving (Typeable)

-- | Smart constructors to keep monads in normal form
infixl 1  `seqK`, `parK`, `syncK`, `seqH`

seqK :: ProgK -> ProgK -> ProgK
seqK (ReturnK {})        m  = m
seqK (SeqK m1 m2)        m3 = seqK m1 (seqK m2 m3)
seqK (LetK v tau e m1)   m2 = letK v tau e (seqK m1 m2)
seqK (BindK v tau m1 m2) m3 = bindK v tau m1 (seqK m2 m3)
seqK m1                  m2 = SeqK m1 m2

parK :: ProgK -> ProgK -> ProgK
parK (ReturnK {}) m  = m
parK (ParK m1 m2) m3 = parK m1 (parK m2 m3)
parK m1           m2 = ParK m1 m2

bindK :: Var -> Type -> ProgK -> ProgK -> ProgK
bindK v  tau  (SeqK m1 m2) m3          = seqK m1 (bindK v tau m2 m3)
bindK v2 tau2 (LetK v1 tau1 e m1)   m2 = letK v1 tau1 e (bindK v2 tau2 m1 m2)
bindK v2 tau2 (BindK v1 tau1 m1 m2) m3 = bindK v1 tau1 m1 (bindK v2 tau2 m2 m3)
bindK v  tau  m1                    m2 = BindK v tau m1 m2

letK :: Var -> Type -> Exp -> ProgK -> ProgK
letK v tau e m = LetK v tau e m

syncK :: ProgK -> ProgK -> ProgK
syncK m1 m2 = m1 `seqK` SyncK `seqK` m2

seqH :: ProgH -> ProgH -> ProgH
seqH (ReturnH {})        m  = m
seqH (SeqH m1 m2)        m3 = seqH m1 (seqH m2 m3)
seqH (LetH v tau e m1)   m2 = letH v tau e (seqH m1 m2)
seqH (BindH v tau m1 m2) m3 = bindH v tau m1 (seqH m2 m3)
seqH m1                  m2 = SeqH m1 m2

letH :: Var -> Type -> Exp -> ProgH -> ProgH
letH v tau e m = LetH v tau e m

bindH :: Var -> Type -> ProgH -> ProgH -> ProgH
bindH v  tau  (SeqH m1 m2) m3          = seqH m1 (bindH v tau m2 m3)
bindH v2 tau2 (BindH v1 tau1 m1 m2) m3 = bindH v1 tau1 m1 (bindH v2 tau2 m2 m3)
bindH v  tau  m1                    m2 = BindH v tau m1 m2

landPrec :: Int
landPrec = 3

lorPrec :: Int
lorPrec = 2

bandPrec :: Int
bandPrec = 7

borPrec :: Int
borPrec = 5

powPrec :: Int
powPrec = 8

eqPrec :: Int
eqPrec = 4

arrowPrec :: Int
arrowPrec = 0

instance Pretty Const where
    ppr (BoolC b)   = ppr b
    ppr (Int8C  i)  = ppr i
    ppr (Int16C i)  = ppr i
    ppr (Int32C i)  = ppr i
    ppr (Int64C i)  = ppr i
    ppr (Word8C  i) = ppr i
    ppr (Word16C i) = ppr i
    ppr (Word32C i) = ppr i
    ppr (Word64C i) = ppr i
    ppr (FloatC f)  = ppr f
    ppr (DoubleC f) = ppr f

instance Show Const where
    showsPrec p = shows . pprPrec p

instance Pretty Unop where
    ppr Lnot    = text "not"

    ppr Ineg    = text "-"
    ppr Iabs    = text "abs"
    ppr Isignum = text "signum"

    ppr Itof    = text "(float)"
    ppr Itod    = text "(double)"

    ppr Fneg    = text "-"
    ppr Fabs    = text "abs"
    ppr Fsignum = text "signum"
    ppr Frecip  = text "1.0/"

    ppr Fexp   = text "expf"
    ppr Fsqrt  = text "sqrtf"
    ppr Flog   = text "logf"
    ppr Fsin   = text "sinf"
    ppr Ftan   = text "tanf"
    ppr Fcos   = text "cosf"
    ppr Fasin  = text "asinf"
    ppr Fatan  = text "atanf"
    ppr Facos  = text "acosf"
    ppr Fsinh  = text "sinhf"
    ppr Ftanh  = text "tanhf"
    ppr Fcosh  = text "coshf"
    ppr Fasinh = text "asinshf"
    ppr Fatanh = text "atanhf"
    ppr Facosh = text "acoshf"

    ppr Dneg    = text "-"
    ppr Dabs    = text "abs"
    ppr Dsignum = text "signum"
    ppr Drecip  = text "1.0/"

    ppr Dexp   = text "exp"
    ppr Dsqrt  = text "sqrt"
    ppr Dlog   = text "log"
    ppr Dsin   = text "sin"
    ppr Dtan   = text "tan"
    ppr Dcos   = text "cos"
    ppr Dasin  = text "asin"
    ppr Datan  = text "atan"
    ppr Dacos  = text "acos"
    ppr Dsinh  = text "sinh"
    ppr Dtanh  = text "tanh"
    ppr Dcosh  = text "cosh"
    ppr Dasinh = text "asinsh"
    ppr Datanh = text "atanh"
    ppr Dacosh = text "acosh"

instance Show Unop where
    showsPrec p = shows . pprPrec p

instance Pretty Binop where
    ppr Land = text "&&"
    ppr Lor  = text "||"

    ppr Leq  = text "=="
    ppr Lne  = text "/="

    ppr Lgt = text ">"
    ppr Lge = text ">="
    ppr Llt = text "<"
    ppr Lle = text "<="

    ppr Band = text "&"
    ppr Bor  = text "|"

    ppr Bmax = text "`max`"
    ppr Bmin = text "`min`"

    ppr Iadd = text "+"
    ppr Isub = text "-"
    ppr Imul = text "*"
    ppr Idiv = text "/"
    ppr Imod = text "%"

    ppr Fadd = text "+"
    ppr Fsub = text "-"
    ppr Fmul = text "*"
    ppr Fdiv = text "/"

    ppr Fpow     = text "**"
    ppr FlogBase = text "`logBase`"

    ppr Dadd = text "+"
    ppr Dsub = text "-"
    ppr Dmul = text "*"
    ppr Ddiv = text "/"

    ppr Dpow     = text "**"
    ppr DlogBase = text "`logBase`"

class HasFixity a where
    fixity :: a -> Fixity

instance HasFixity Unop where
    fixity Lnot    = infix_ appPrec

    fixity Ineg    = infix_ addPrec
    fixity Iabs    = infix_ appPrec
    fixity Isignum = infix_ appPrec

    fixity Itof    = infix_ appPrec
    fixity Itod    = infix_ appPrec

    fixity Fneg    = infix_ addPrec
    fixity Fabs    = infix_ appPrec
    fixity Fsignum = infix_ appPrec
    fixity Frecip  = infix_ appPrec

    fixity Fexp   = infix_ appPrec
    fixity Fsqrt  = infix_ appPrec
    fixity Flog   = infix_ appPrec
    fixity Fsin   = infix_ appPrec
    fixity Ftan   = infix_ appPrec
    fixity Fcos   = infix_ appPrec
    fixity Fasin  = infix_ appPrec
    fixity Fatan  = infix_ appPrec
    fixity Facos  = infix_ appPrec
    fixity Fsinh  = infix_ appPrec
    fixity Ftanh  = infix_ appPrec
    fixity Fcosh  = infix_ appPrec
    fixity Fasinh = infix_ appPrec
    fixity Fatanh = infix_ appPrec
    fixity Facosh = infix_ appPrec

    fixity Dneg    = infix_ addPrec
    fixity Dabs    = infix_ appPrec
    fixity Dsignum = infix_ appPrec
    fixity Drecip  = infix_ appPrec

    fixity Dexp   = infix_ appPrec
    fixity Dsqrt  = infix_ appPrec
    fixity Dlog   = infix_ appPrec
    fixity Dsin   = infix_ appPrec
    fixity Dtan   = infix_ appPrec
    fixity Dcos   = infix_ appPrec
    fixity Dasin  = infix_ appPrec
    fixity Datan  = infix_ appPrec
    fixity Dacos  = infix_ appPrec
    fixity Dsinh  = infix_ appPrec
    fixity Dtanh  = infix_ appPrec
    fixity Dcosh  = infix_ appPrec
    fixity Dasinh = infix_ appPrec
    fixity Datanh = infix_ appPrec
    fixity Dacosh = infix_ appPrec

instance HasFixity Binop where
    fixity Land = infixr_ landPrec
    fixity Lor  = infixr_ lorPrec

    fixity Leq = infix_ eqPrec
    fixity Lne = infix_ eqPrec
    fixity Lgt = infix_ eqPrec
    fixity Lge = infix_ eqPrec
    fixity Llt = infix_ eqPrec
    fixity Lle = infix_ eqPrec

    fixity Band = infixl_ bandPrec
    fixity Bor  = infixl_ borPrec

    fixity Bmax     = infixl_ 9
    fixity Bmin     = infixl_ 9

    fixity Iadd = infixl_ addPrec
    fixity Isub = infixl_ addPrec
    fixity Imul = infixl_ mulPrec
    fixity Idiv = infixl_ mulPrec
    fixity Imod = infixl_ mulPrec

    fixity Fadd = infixl_ addPrec
    fixity Fsub = infixl_ addPrec
    fixity Fmul = infixl_ mulPrec
    fixity Fdiv = infixl_ mulPrec

    fixity Fpow     = infixr_ powPrec
    fixity FlogBase = infixl_ mulPrec

    fixity Dadd = infixl_ addPrec
    fixity Dsub = infixl_ addPrec
    fixity Dmul = infixl_ mulPrec
    fixity Ddiv = infixl_ mulPrec

    fixity Dpow     = infixr_ powPrec
    fixity DlogBase = infixl_ mulPrec

instance Show Binop where
    showsPrec p = shows . pprPrec p

instance Pretty Var where
    ppr (Var v) = text v

instance Show Var where
    showsPrec p = shows . pprPrec p

instance Pretty ScalarType where
    pprPrec _ UnitT         = text "()"
    pprPrec _ BoolT         = text "Bool"
    pprPrec _ Int8T         = text "Int8"
    pprPrec _ Int16T        = text "Int16"
    pprPrec _ Int32T        = text "Int32"
    pprPrec _ Int64T        = text "Int64"
    pprPrec _ Word8T        = text "Word8"
    pprPrec _ Word16T       = text "Word16"
    pprPrec _ Word32T       = text "Word32"
    pprPrec _ Word64T       = text "Word64"
    pprPrec _ FloatT        = text "Float"
    pprPrec _ DoubleT       = text "Double"
    pprPrec _ (TupleT taus) = tuple (map ppr taus)

instance Show ScalarType where
    showsPrec p = shows . pprPrec p

instance Pretty Type where
    pprPrec p (ScalarT tau) =
        pprPrec p tau

    pprPrec p (ArrayT tau sh) =
        parensIf (p > appPrec) $
        text "Array" <+> ppr tau <> brackets (text (replicate (sh-1) ','))

    pprPrec p (FunT taus tau) =
        parensIf (p > arrowPrec) $
        funargs (map ppr taus) <+>
        text "->" <+>
        pprPrec arrowPrec tau
      where
        funargs [x] = x
        funargs xs  = parens (commasep xs)

    pprPrec p (MT tau) =
        parensIf (p > appPrec) $
        text "M" <+> pprPrec appPrec1 tau

instance Show Type where
    showsPrec p = shows . pprPrec p

instance Pretty (Var, Type) where
    ppr (v, tau) =
        ppr v <+> text "::" <+> ppr tau

instance Show (Var, Type) where
    showsPrec p = shows . pprPrec p

instance Pretty Exp where
    pprPrec _ (VarE v) =
        ppr v

    pprPrec _ (ConstE c) =
        ppr c

    pprPrec _ UnitE =
        ppr ()

    pprPrec _ (TupleE es) =
        tuple (map ppr es)

    pprPrec p (ProjE i _ e) =
        parensIf (p > appPrec) $
        text "#" <> ppr i <+> pprPrec appPrec1 e

    pprPrec p (ProjArrE i _ e) =
        parensIf (p > appPrec) $
        text "#" <> ppr i <+> pprPrec appPrec1 e

    pprPrec p (DimE i _ e) =
        parensIf (p > appPrec) $
        text "dim#" <> ppr i <+> pprPrec appPrec1 e

    pprPrec p (LetE v tau Many e1 e2) =
        parensIf (p > appPrec) $
        text "let" <+> text "{" <+>
          nest 2 (ppr (v, tau) <+> text "=" <+> ppr e1) <+> text "}" <+>
        text "in" </>
        ppr e2

    pprPrec p (LetE v tau occ e1 e2) =
        parensIf (p > appPrec) $
        text "let" <+> text "{" <+>
          nest 2 (ppr (v, tau) <+> parens (ppr occ) <+> text "=" <+> ppr e1) <+> text "}" <+>
        text "in" </>
        ppr e2

    pprPrec p (LamE vtaus e) =
        parensIf (p > appPrec) $ nest 2 $
        text "\\" <> tuple (map ppr vtaus) <+> text "->" </> ppr e

    pprPrec p (AppE f es) =
        parensIf (p > appPrec) $
        folddoc (<+/>) (map (pprPrec appPrec1) (f : es))

    pprPrec p (UnopE op e) =
        parensIf (p >= opp) $
        ppr op <> unopSpace op <> pprPrec (opp+1) e
      where
        Fixity _ opp = fixity op

        unopSpace :: Unop -> Doc
        unopSpace Ineg   = empty
        unopSpace Fneg   = empty
        unopSpace Dneg   = empty
        unopSpace Frecip = empty
        unopSpace Drecip = empty
        unopSpace _      = space

    pprPrec p (BinopE FlogBase e1 e2) =
        parensIf (p > appPrec) $
        text "logBase" <+>
        pprPrec appPrec1 e1 <+>
        pprPrec appPrec1 e2

    pprPrec p (BinopE Bmax e1 e2) =
        parensIf (p > appPrec) $
        text "max" <+> pprPrec appPrec1 e1 <+> pprPrec appPrec1 e2

    pprPrec p (BinopE Bmin e1 e2) =
        parensIf (p > appPrec) $
        text "min" <+> pprPrec appPrec1 e1 <+> pprPrec appPrec1 e2

    pprPrec p (BinopE op e1 e2) =
        infixop p (fixity op) (ppr op) e1 e2

    pprPrec p (IfThenElseE teste thene elsee) =
        parensIf (p > appPrec) $ align $
        text "if" <+> ppr teste </>
        text "then" <+> align (ppr thene) </>
        text "else" <+> align (ppr elsee)

    pprPrec p (SwitchE e cases dflt) =
        parensIf (p > appPrec) $
        align $
        text "case" <+>
        align (ppr e <+> text "of") </>
        embraceStack (map pprAlt alts)
      where
        alts :: [(Doc, Doc)]
        alts = [(ppr i, ppr e) | (i, e) <- cases] ++
               case dflt of
                 Nothing -> []
                 Just e  -> [(text "_", ppr e)]

        pprAlt :: (Doc, Doc) -> Doc
        pprAlt (p, e) = p <+> text "->" <+> align  e

    pprPrec _ (IndexE arr idx) =
        pprPrec appPrec1 arr <> brackets (ppr idx)

    pprPrec _ (DelayedE _) =
        text "<delayed>"

instance Show Exp where
    showsPrec p = shows . pprPrec p

instance Pretty ProgH where
    ppr prog =
        case (go prog) of
          [p] -> p
          ps  -> embraceStack ps
      where
        go :: ProgH -> [Doc]
        go (ReturnH e) =
            [text "return" <+> pprPrec appPrec1 e]

        go (SeqH m1 m2) =
            ppr m1 : go m2

        go  (LetH v tau e m) =
            (text "let" <+> ppr (v, tau) <+> text "=" <+> ppr e) : go m

        go  (BindH v tau m1 m2) =
            (ppr (v, tau) <+> text "<-" <+/> nest 2 (ppr m1)) : go m2

        go (IfThenElseH testp thenp elsep) =
            [align $ text "if" <+> ppr testp </>
                     text "then" <+> align (ppr thenp) </>
                     text "else" <+> align (ppr elsep)]

        go (LiftH proc args) =
            [text "lift" <+>
                  pprPrec appPrec1 proc <+>
                  tuple (map ppr args)]

        go (AllocH tau sh) =
            [text "alloc" <+> pprPrec appPrec1 tau <> brackets (commasep (map ppr sh))]

        go (DelayedH {}) =
            [text "<delayed>"]

instance Show ProgH where
    showsPrec p = shows . pprPrec p

instance Pretty ProcH where
    pprPrec p (ProcH bndrs proc) =
        parensIf (p > appPrec) $ nest 2 $
        text "\\" <> tuple (map ppr bndrs) <+> text "->" </> ppr proc

instance Show ProcH where
    showsPrec p = shows . pprPrec p

instance Pretty ProgK where
    ppr prog =
        case (go prog) of
          [p] -> p
          ps  -> embraceStack ps
      where
        go :: ProgK -> [Doc]
        go (ReturnK e) =
            [text "return" <+> pprPrec appPrec1 e]

        go (SeqK m1 m2) =
            ppr m1 : go m2

        go (ParK m1 m2) =
            [align (ppr m1 </> text "||" </> ppr m2)]

        go  (LetK v tau e m) =
            (text "let" <+> ppr (v, tau) <+> text "=" <+> ppr e) : go m

        go  (BindK v tau m1 m2) =
            (ppr (v, tau) <+> text "<-" <+/> nest 2 (ppr m1)) : go m2

        go (ForK vs es prog) =
            [align $ nest 4 $
             text "for" <> parens
                 (commasep (replicate (length vs) (text "0")) <+> text "<=" <+>
                  commasep (map ppr vs) <+> text "<" <+>
                  commasep (map ppr es)) </>
             ppr prog]

        go (ParforK vs es prog) =
            [align $ nest 4 $
             text "parfor" <> parens
                 (commasep (replicate (length vs) (text "0")) <+> text "<=" <+>
                  commasep (map ppr vs) <+> text "<" <+>
                  commasep (map ppr es)) </>
             ppr prog]

        go (IfThenElseK testp thenp elsep) =
            [align $ text "if" <+> ppr testp </>
                     text "then" <+> align (ppr thenp) </>
                     text "else" <+> align (ppr elsep)]

        go (WriteK v idx x) =
            [text "write" <+> pprPrec appPrec1 v <>
             brackets (ppr idx) <+> pprPrec appPrec1 x]

        go SyncK =
            [text "sync"]

        go (DelayedK {}) =
            [text "<delayed>"]

instance Show ProgK where
    showsPrec p = shows . pprPrec p

instance Pretty ProcK where
    pprPrec p (ProcK bndrs proc) =
        parensIf (p > appPrec) $ nest 2 $
        text "\\" <> tuple (map ppr bndrs) <+> text "->" </> ppr proc

instance Show ProcK where
    showsPrec p = shows . pprPrec p

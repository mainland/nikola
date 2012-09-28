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
    isUnitT,
    isArrayT,
    isFunT,
    isMT,
    flattenT,

    Exp(..),
    splitLamE,

    seqE,
    parE,
    bindE,
    syncE
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
data Unop = -- Casting
            Cast ScalarType

            -- Logical operators
          | NotL

            -- Numeric operators
          | NegN
          | AbsN
          | SignumN

            -- Floating point operators
          | RecipF
          | ExpF
          | SqrtF
          | LogF
          | SinF
          | TanF
          | CosF
          | AsinF
          | AtanF
          | AcosF
          | SinhF
          | TanhF
          | CoshF
          | AsinhF
          | AtanhF
          | AcoshF
  deriving (Eq, Ord, Data, Typeable)

-- | Binary operators
data Binop = -- Order operators
             EqO
           | NeO
           | GtO
           | GeO
           | LtO
           | LeO

           | MaxO
           | MinO

             -- Logical operators
           | AndL
           | OrL

            -- Numeric operators
           | AddN
           | SubN
           | MulN
           | DivN

             -- Bitwise operators
           | AndB
           | OrB

            -- Integral operators
           | ModI

            -- Floating point operators
           | PowF
           | LogBaseF
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

isUnitT :: Type -> Bool
isUnitT (ScalarT UnitT) = True
isUnitT _               = False

isArrayT :: Type -> Bool
isArrayT (ArrayT {}) = True
isArrayT _           = False

isFunT :: Type -> Bool
isFunT (FunT {}) = True
isFunT _         = False

isMT :: Type -> Bool
isMT (MT {}) = True
isMT _       = False

flattenT :: Type -> [Type]
flattenT (ScalarT (TupleT taus)) =
    concatMap flattenT (map ScalarT taus)

flattenT tau =
    [tau]

-- Occurence information
data Occ = Never      -- ^ Never occurs
         | Once       -- ^ Occurs once
         | ManyBranch -- ^ Occurs once but in many branches
         | Many       -- ^ Occurs many times
  deriving (Eq, Ord, Show)

instance Pretty Occ where
    ppr = text . show

-- Combine occurences in paths of which both are executed
occJoin :: Occ -> Occ -> Occ
occJoin Never      occ   = occ
occJoin occ        Never = occ
occJoin Many       _     = Many
occJoin _          Many  = Many

occJoin Once       _     = Many
occJoin ManyBranch _     = Many

-- Combine occurences in paths of which only one is executed
occMeet :: Occ -> Occ -> Occ
occMeet Never      occ         = occ
occMeet occ        Never       = occ
occMeet Many       _           = Many
occMeet _          Many        = Many

occMeet Once       Once        = ManyBranch
occMeet Once       ManyBranch  = ManyBranch
occMeet ManyBranch Once        = ManyBranch
occMeet ManyBranch ManyBranch  = ManyBranch

-- | Expressions. These can be evaluated either on the host or the device.
data Exp = VarE Var
         | ConstE Const
         | UnitE

         | TupleE [Exp]
         | ProjE Int Int Exp

         | LetE Var Type Occ Exp Exp

         | LamE [(Var, Type)] Exp
         | AppE Exp [Exp]
         | CallE Exp [Exp]

         | UnopE Unop Exp
         | BinopE Binop Exp Exp

         | IfThenElseE Exp Exp Exp

         | SwitchE Exp [(Int, Exp)] (Maybe Exp)

         | ReturnE Exp
         | SeqE Exp Exp
         | ParE Exp Exp
         | BindE Var Type Exp Exp

         | AllocE Type [Exp]
         | DimE Int Int Exp
         | ProjArrE Int Int Exp
         | IndexE Exp Exp
         | WriteE Exp Exp Exp

         | ForE Bool [Var] [Exp] Exp

         | SyncE

         | DelayedE (R Exp Exp)
  deriving (Eq, Ord, Typeable)

instance Eq (R Exp Exp) where
    _ == _ = error "R Exp Exp: incomparable"

instance Ord (R Exp Exp) where
    compare _  _ = error "R Exp Exp: incomparable"

instance Num Exp where
    e1 + e2 = BinopE AddN e1 e2
    e1 - e2 = BinopE SubN e1 e2
    e1 * e2 = BinopE MulN e1 e2

    negate e    = UnopE NegN e
    fromInteger = ConstE . Int32C . fromInteger

    abs e    = UnopE AbsN e
    signum e = UnopE SignumN e

splitLamE :: Exp -> ([(Var, Type)], Exp)
splitLamE (LamE vtaus e) = (vtaus, e)
splitLamE e              = ([], e)

-- | Smart constructors to keep monads in normal form
infixl 1  `seqE`, `parE`, `syncE`

seqE :: Exp -> Exp -> Exp
seqE (ReturnE {})        m  = m
seqE (SeqE m1 m2)        m3 = seqE m1 (seqE m2 m3)
seqE (LetE v tau _ e m1) m2 = letE v tau e (seqE m1 m2)
seqE (BindE v tau m1 m2) m3 = bindE v tau m1 (seqE m2 m3)
seqE m1                  m2 = SeqE m1 m2

parE :: Exp -> Exp -> Exp
parE (ReturnE {}) m  = m
parE (ParE m1 m2) m3 = parE m1 (parE m2 m3)
parE m1           m2 = ParE m1 m2

bindE :: Var -> Type -> Exp -> Exp -> Exp
bindE v  tau  (SeqE m1 m2) m3          = seqE m1 (bindE v tau m2 m3)
bindE v2 tau2 (LetE v1 tau1 _ e m1) m2 = letE v1 tau1 e (bindE v2 tau2 m1 m2)
bindE v2 tau2 (BindE v1 tau1 m1 m2) m3 = bindE v1 tau1 m1 (bindE v2 tau2 m2 m3)
bindE v  tau  m1                    m2 = BindE v tau m1 m2

letE :: Var -> Type -> Exp -> Exp -> Exp
letE v tau e m = LetE v tau Many e m

syncE :: Exp -> Exp -> Exp
syncE m1 m2 = m1 `seqE` SyncE `seqE` m2

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
    ppr (Cast Int8T)   = text "(int8_t)"
    ppr (Cast Int16T)  = text "(int16_t)"
    ppr (Cast Int32T)  = text "(int32_t)"
    ppr (Cast Int64T)  = text "(int64_t)"
    ppr (Cast Word8T)  = text "(uint8_t)"
    ppr (Cast Word16T) = text "(uint16_t)"
    ppr (Cast Word32T) = text "(uint32_t)"
    ppr (Cast Word64T) = text "(uint64_t)"
    ppr (Cast FloatT)  = text "(float)"
    ppr (Cast DoubleT) = text "(double)"
    ppr (Cast tau)     = errordoc $ text "Bad cast to" <+> ppr tau

    ppr NotL    = text "not"

    ppr NegN    = text "-"
    ppr AbsN    = text "abs"
    ppr SignumN = text "signum"

    ppr RecipF  = text "1.0/"

    ppr ExpF   = text "exp"
    ppr SqrtF  = text "sqrt"
    ppr LogF   = text "log"
    ppr SinF   = text "sin"
    ppr TanF   = text "tan"
    ppr CosF   = text "cos"
    ppr AsinF  = text "asin"
    ppr AtanF  = text "atan"
    ppr AcosF  = text "acos"
    ppr SinhF  = text "sinh"
    ppr TanhF  = text "tanh"
    ppr CoshF  = text "cosh"
    ppr AsinhF = text "asinsh"
    ppr AtanhF = text "atanh"
    ppr AcoshF = text "acosh"

instance Show Unop where
    showsPrec p = shows . pprPrec p

instance Pretty Binop where
    ppr EqO = text "=="
    ppr NeO = text "/="
    ppr GtO = text ">"
    ppr GeO = text ">="
    ppr LtO = text "<"
    ppr LeO = text "<="

    ppr MaxO = text "`max`"
    ppr MinO = text "`min`"

    ppr AndL = text "&&"
    ppr OrL  = text "||"

    ppr AddN = text "+"
    ppr SubN = text "-"
    ppr MulN = text "*"
    ppr DivN = text "/"

    ppr AndB = text "&"
    ppr OrB  = text "|"

    ppr ModI = text "%"

    ppr PowF     = text "**"
    ppr LogBaseF = text "`logBase`"

class HasFixity a where
    fixity :: a -> Fixity

instance HasFixity Unop where
    fixity (Cast _) = infix_ appPrec

    fixity NotL = infix_ appPrec

    fixity NegN    = infix_ addPrec
    fixity AbsN    = infix_ appPrec
    fixity SignumN = infix_ appPrec

    fixity RecipF = infix_ appPrec
    fixity ExpF   = infix_ appPrec
    fixity SqrtF  = infix_ appPrec
    fixity LogF   = infix_ appPrec
    fixity SinF   = infix_ appPrec
    fixity TanF   = infix_ appPrec
    fixity CosF   = infix_ appPrec
    fixity AsinF  = infix_ appPrec
    fixity AtanF  = infix_ appPrec
    fixity AcosF  = infix_ appPrec
    fixity SinhF  = infix_ appPrec
    fixity TanhF  = infix_ appPrec
    fixity CoshF  = infix_ appPrec
    fixity AsinhF = infix_ appPrec
    fixity AtanhF = infix_ appPrec
    fixity AcoshF = infix_ appPrec

instance HasFixity Binop where
    fixity EqO = infix_ eqPrec
    fixity NeO = infix_ eqPrec
    fixity GtO = infix_ eqPrec
    fixity GeO = infix_ eqPrec
    fixity LtO = infix_ eqPrec
    fixity LeO = infix_ eqPrec

    fixity MaxO = infixl_ 9
    fixity MinO = infixl_ 9

    fixity AndL = infixr_ landPrec
    fixity OrL  = infixr_ lorPrec

    fixity AddN = infixl_ addPrec
    fixity SubN = infixl_ addPrec
    fixity MulN = infixl_ mulPrec
    fixity DivN = infixl_ mulPrec

    fixity AndB = infixl_ bandPrec
    fixity OrB  = infixl_ borPrec

    fixity ModI = infixl_ mulPrec

    fixity PowF     = infixr_ powPrec
    fixity LogBaseF = infixl_ mulPrec

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

    pprPrec p (CallE f es) =
        parensIf (p > appPrec) $
        text "call" <+> folddoc (<+/>) (pprMonadic appPrec1 f : map (pprPrec appPrec1) es)

    pprPrec p (UnopE op e) =
        parensIf (p >= opp) $
        ppr op <> unopSpace op <> pprPrec (opp+1) e
      where
        Fixity _ opp = fixity op

        unopSpace :: Unop -> Doc
        unopSpace NegN   = empty
        unopSpace RecipF = empty
        unopSpace _      = space

    pprPrec p (BinopE MaxO e1 e2) =
        parensIf (p > appPrec) $
        text "max" <+> pprPrec appPrec1 e1 <+> pprPrec appPrec1 e2

    pprPrec p (BinopE MinO e1 e2) =
        parensIf (p > appPrec) $
        text "min" <+> pprPrec appPrec1 e1 <+> pprPrec appPrec1 e2

    pprPrec p (BinopE LogBaseF e1 e2) =
        parensIf (p > appPrec) $
        text "logBase" <+>
        pprPrec appPrec1 e1 <+>
        pprPrec appPrec1 e2

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

    pprPrec p e@(ReturnE {}) =
        pprMonadic p e

    pprPrec p e@(SeqE {}) =
        pprMonadic p e

    pprPrec p e@(ParE {}) =
        pprMonadic p e

    pprPrec p e@(BindE {}) =
        pprMonadic p e

    pprPrec p e@(AllocE {}) =
        pprMonadic p e

    pprPrec p (DimE i _ e) =
        parensIf (p > appPrec) $
        text "dim#" <> ppr i <+> pprPrec appPrec1 e

    pprPrec p (ProjArrE i _ e) =
        parensIf (p > appPrec) $
        text "#" <> ppr i <+> pprPrec appPrec1 e

    pprPrec _ (IndexE arr idx) =
        pprPrec appPrec1 arr <> brackets (ppr idx)

    pprPrec p e@(WriteE {}) =
        pprMonadic p e

    pprPrec p e@(ForE {}) =
        pprMonadic p e

    pprPrec p e@(SyncE {}) =
        pprMonadic p e

    pprPrec _ (DelayedE _) =
        text "<delayed>"

instance Show Exp where
    showsPrec p = shows . pprPrec p

pprMonadic :: Int -> Exp -> Doc
pprMonadic p (LamE vtaus e) =
    parensIf (p > appPrec) $ nest 2 $
    text "\\" <> tuple (map ppr vtaus) <+> text "->" </> pprMonadic 0 e

pprMonadic _ e =
    case go e of
      [m] -> m
      ms  -> embraceStack ms
  where
    go :: Exp -> [Doc]
    go (LetE v tau _ e m) =
        (text "let" <+> ppr (v, tau) <+> text "=" <+> ppr e) : go m

    go (LamE vtaus e) =
        [nest 2 $ text "\\" <> tuple (map ppr vtaus) <+> text "->" </> pprMonadic 0 e]

    go (CallE f es) =
        [text "call" <+> pprMonadic appPrec1 f <+> tuple (map ppr es)]

    go (ReturnE e) =
        [text "return" <+> pprPrec appPrec1 e]

    go (SeqE m1 m2) =
        ppr m1 : go m2

    go (ParE m1 m2) =
        [align (ppr m1 </> text "||" </> ppr m2)]

    go (BindE v tau m1 m2) =
        (ppr (v, tau) <+> text "<-" <+/> nest 2 (ppr m1)) : go m2

    go (AllocE tau sh) =
        [text "alloc" <+> pprPrec appPrec1 tau <> brackets (commasep (map ppr sh))]

    go (WriteE v idx x) =
        [text "write" <+> pprPrec appPrec1 v <>
         brackets (ppr idx) <+> pprPrec appPrec1 x]

    go (ForE isPar vs es prog) =
        [align $ nest 4 $
         (if isPar then text "parfor" else text "for") <>
         parens (commasep (replicate (length vs) (text "0")) <+> text "<=" <+>
                 commasep (map ppr vs) <+> text "<" <+>
                 commasep (map ppr es)) </>
         pprMonadic 0 prog]

    go SyncE =
        [text "sync"]

    go e =
        [ppr e]

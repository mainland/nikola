{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Data.Array.Nikola.Language.Check
-- Copyright   : (c) Geoffrey Mainland 2012
-- License     : BSD-style
--
-- Maintainer  : Geoffrey Mainland <mainland@apeiron.net>
-- Stability   : experimental
-- Portability : non-portable

module Data.Array.Nikola.Language.Check (
    Context(..),
    MonadCheck(..),

    inContext,
    ifHostContext,
    ifKernelContext,

    inferExp,
    checkExp,

    isIntT,
    isFloatT,
    isNumT,

    checkScalarT,
    checkArrayT,
    checkFunT,
    checkMT,

    checkTraverseFam
  ) where

import Prelude hiding (mapM)

import Control.Applicative (Applicative, (<$>), (<*>), pure)
import Control.Monad ((>=>),
                      void,
                      when)
import Control.Monad.Trans (MonadIO(..))
import Data.Traversable
import Text.PrettyPrint.Mainland

import Data.Array.Nikola.Language.Generic
import Data.Array.Nikola.Language.Syntax

-- | Current context
data Context = Host
             | Kernel
  deriving (Eq, Ord, Show)

instance Pretty Context where
    ppr = text . show

class (Functor m, Applicative m, Monad m, MonadIO m) => MonadCheck m where
    getContext :: m Context
    setContext :: Context -> m ()

    lookupVarType  :: Var -> m Type
    extendVarTypes :: [(Var, Type)] -> m a -> m a

inContext :: MonadCheck m => Context -> m a -> m a
inContext ctx act = do
    old_ctx <- getContext
    setContext ctx
    a <- act
    setContext old_ctx
    return a

ifHostContext :: MonadCheck m => m () -> m ()
ifHostContext act = do
    ctx <- getContext
    when (ctx == Host) act

ifKernelContext :: MonadCheck m => m () -> m ()
ifKernelContext act = do
    ctx <- getContext
    when (ctx == Kernel) act

isIntT :: Type -> Bool
isIntT (ScalarT Int8T)   = True
isIntT (ScalarT Int16T)  = True
isIntT (ScalarT Int32T)  = True
isIntT (ScalarT Int64T)  = True
isIntT (ScalarT Word8T)  = True
isIntT (ScalarT Word16T) = True
isIntT (ScalarT Word32T) = True
isIntT (ScalarT Word64T) = True
isIntT _                 = False

isFloatT :: Type -> Bool
isFloatT (ScalarT FloatT)  = True
isFloatT (ScalarT DoubleT) = True
isFloatT _                 = False

isNumT :: Type -> Bool
isNumT tau = isIntT tau || isFloatT tau

joinBoolT :: MonadCheck m => Type -> Type -> m Type
joinBoolT tau@(ScalarT BoolT) (ScalarT BoolT) = return tau

joinBoolT tau1 tau2 =
    faildoc $ text "Expected" <+> ppr tau2 <+>
              text "but got" <+> ppr tau1

joinIntT :: MonadCheck m => Type -> Type -> m Type
joinIntT tau@(ScalarT Int8T)   (ScalarT Int8T)   = return tau
joinIntT tau@(ScalarT Int16T)  (ScalarT Int16T)  = return tau
joinIntT tau@(ScalarT Int32T)  (ScalarT Int32T)  = return tau
joinIntT tau@(ScalarT Int64T)  (ScalarT Int64T)  = return tau
joinIntT tau@(ScalarT Word8T)  (ScalarT Word8T)  = return tau
joinIntT tau@(ScalarT Word16T) (ScalarT Word16T) = return tau
joinIntT tau@(ScalarT Word32T) (ScalarT Word32T) = return tau
joinIntT tau@(ScalarT Word64T) (ScalarT Word64T) = return tau

joinIntT tau1 tau2 =
    faildoc $ text "Expected" <+> ppr tau2 <+>
              text "but got" <+> ppr tau1

joinFloatT :: MonadCheck m => Type -> Type -> m Type
joinFloatT tau@(ScalarT FloatT)  (ScalarT FloatT)  = return tau
joinFloatT tau@(ScalarT DoubleT) (ScalarT DoubleT) = return tau
joinFloatT tau1 tau2 =
    faildoc $ text "Expected" <+> ppr tau2 <+>
              text "but got" <+> ppr tau1

joinNumT :: MonadCheck m => Type -> Type -> m Type
joinNumT tau@(ScalarT Int8T)   (ScalarT Int8T)   = return tau
joinNumT tau@(ScalarT Int16T)  (ScalarT Int16T)  = return tau
joinNumT tau@(ScalarT Int32T)  (ScalarT Int32T)  = return tau
joinNumT tau@(ScalarT Int64T)  (ScalarT Int64T)  = return tau
joinNumT tau@(ScalarT Word8T)  (ScalarT Word8T)  = return tau
joinNumT tau@(ScalarT Word16T) (ScalarT Word16T) = return tau
joinNumT tau@(ScalarT Word32T) (ScalarT Word32T) = return tau
joinNumT tau@(ScalarT Word64T) (ScalarT Word64T) = return tau
joinNumT tau@(ScalarT FloatT)  (ScalarT FloatT)  = return tau
joinNumT tau@(ScalarT DoubleT) (ScalarT DoubleT) = return tau

joinNumT tau1 tau2 =
    faildoc $ text "Expected" <+> ppr tau2 <+>
              text "but got" <+> ppr tau1

checkEqT :: MonadCheck m => Type -> Type -> m ()
checkEqT tau1 tau2
    | tau1 == tau2 = return ()
    | otherwise    = faildoc $
                     text "Expected" <+> ppr tau2 <+>
                     text "but got" <+> ppr tau1

checkIntT :: MonadCheck m => Type -> m ()
checkIntT tau = do
    when (not (isIntT tau)) $ do
        faildoc $ text "Expected integer type but got" <+> ppr tau

checkScalarT :: forall m . MonadCheck m => Type -> m ScalarType
checkScalarT (ScalarT tau) = return tau
checkScalarT tau           = faildoc $
                             text "Expected scalar type but got" <+> ppr tau

checkArrayT :: MonadCheck m => Type -> m (ScalarType, Int)
checkArrayT (ArrayT tau n) = return (tau, n)
checkArrayT tau            = faildoc $
                             text "Expected array type but got" <+> ppr tau

checkFunT :: MonadCheck m => Type -> m ([Type], Type)
checkFunT (FunT taus tau) = return (taus, tau)
checkFunT tau             = faildoc $
                            text "Expected function type but got" <+> ppr tau

checkMT :: MonadCheck m => Type -> m Type
checkMT (MT tau) = return tau
checkMT tau      = faildoc $
                   text "Expected monadic type but got" <+> ppr tau

inferConst :: forall m . MonadCheck m => Const -> m ScalarType
inferConst = go
  where
    go :: Const -> m ScalarType
    go (BoolC {})   = return BoolT
    go (Int8C {})   = return Int8T
    go (Int16C {})  = return Int16T
    go (Int32C {})  = return Int32T
    go (Int64C {})  = return Int64T
    go (Word8C {})  = return Word8T
    go (Word16C {}) = return Word16T
    go (Word32C {}) = return Word32T
    go (Word64C {}) = return Word64T
    go (FloatC {})  = return FloatT
    go (DoubleC {}) = return DoubleT

inferUnop :: forall m . MonadCheck m => Unop -> Type -> m Type
inferUnop = go
  where
    go :: Unop -> Type -> m Type

    go NotL (ScalarT BoolT) = return boolT

    go (Cast tau1) tau2 | isNumT (ScalarT tau1) && isNumT tau2 =
        return (ScalarT tau1)

    go NegN    tau | isNumT tau   = return tau
    go AbsN    tau | isNumT tau   = return tau
    go SignumN tau | isNumT tau   = return tau

    go RecipF  tau | isFloatT tau = return tau
    go ExpF    tau | isFloatT tau = return tau
    go SqrtF   tau | isFloatT tau = return tau
    go LogF    tau | isFloatT tau = return tau
    go SinF    tau | isFloatT tau = return tau
    go TanF    tau | isFloatT tau = return tau
    go CosF    tau | isFloatT tau = return tau
    go AsinF   tau | isFloatT tau = return tau
    go AtanF   tau | isFloatT tau = return tau
    go AcosF   tau | isFloatT tau = return tau
    go SinhF   tau | isFloatT tau = return tau
    go TanhF   tau | isFloatT tau = return tau
    go CoshF   tau | isFloatT tau = return tau
    go AsinhF  tau | isFloatT tau = return tau
    go AtanhF  tau | isFloatT tau = return tau
    go AcoshF  tau | isFloatT tau = return tau

    go RoundF  tau | isFloatT tau = return (ScalarT Int32T)
    go CeilF   tau | isFloatT tau = return (ScalarT Int32T)
    go FloorF  tau | isFloatT tau = return (ScalarT Int32T)

    go op tau =
        faildoc $
        text "Operator" <+> ppr op <+>
        text "does not accept arguments of type" <+> ppr tau

inferBinop :: forall m . MonadCheck m => Binop -> Type -> Type -> m Type
inferBinop =
    go
  where
    go :: Binop -> Type -> Type -> m Type
    go EqO tau1 tau2      = joinNumT tau1 tau2 >> return boolT
    go NeO tau1 tau2      = joinNumT tau1 tau2 >> return boolT
    go GtO tau1 tau2      = joinNumT tau1 tau2 >> return boolT
    go GeO tau1 tau2      = joinNumT tau1 tau2 >> return boolT
    go LtO tau1 tau2      = joinNumT tau1 tau2 >> return boolT
    go LeO tau1 tau2      = joinNumT tau1 tau2 >> return boolT

    go MaxO tau1 tau2     = joinNumT tau1 tau2
    go MinO tau1 tau2     = joinNumT tau1 tau2

    go AndL tau1 tau2     = joinBoolT tau1 tau2
    go OrL tau1 tau2      = joinBoolT tau1 tau2

    go AddN tau1 tau2     = joinNumT tau1 tau2
    go SubN tau1 tau2     = joinNumT tau1 tau2
    go MulN tau1 tau2     = joinNumT tau1 tau2

    go AndB tau1 tau2     = joinIntT tau1 tau2
    go OrB  tau1 tau2     = joinIntT tau1 tau2

    go QuotI tau1 tau2    = joinIntT tau1 tau2
    go RemI  tau1 tau2    = joinIntT tau1 tau2

    go DivF     tau1 tau2 = joinFloatT tau1 tau2
    go ModF     tau1 tau2 = joinFloatT tau1 tau2
    go PowF     tau1 tau2 = joinFloatT tau1 tau2
    go LogBaseF tau1 tau2 = joinFloatT tau1 tau2

inferExp :: forall m . MonadCheck m => Exp -> m Type
inferExp = go
  where
    go (VarE v) =
        lookupVarType v

    go (ConstE c) =
        ScalarT <$> inferConst c

    go UnitE =
        return unitT

    go (TupleE es) = do
        taus <- mapM (inferExp >=> checkScalarT) es
        return $ ScalarT (TupleT taus)

    go (ProjE i n e) = do
        tau <- inferExp e
        case tau of
          ScalarT (TupleT taus) | length taus == n ->
              return $ ScalarT (taus !! i)
          _ ->
              faildoc $ text "Expected tuple type with" <+> ppr n <+>
                        text "components but got" <+> ppr tau

    go (LetE v tau _ e1 e2) = do
        tau' <- inferExp e1
        when (tau' /= tau) $
            faildoc $ text "Type mis-match in let binding of" <+> ppr v
        extendVarTypes [(v, tau)] $ inferExp e2

    go (LamE vtaus e) = do
        tau  <- extendVarTypes vtaus $
                inferExp e
        return $ FunT (map snd vtaus) tau

    go e@(AppE f es) = do
        (taus, tau) <- inferExp f >>= checkFunT
        taus'       <- mapM inferExp es
        when (taus' /= taus) $ do
            faildoc $ text "Type mis-match in function call" <+>
                      ppr e
        return tau

    go e@(CallE f es) = do
        (taus, mtau) <- inContext Kernel (inferExp f) >>= checkFunT
        void $ checkMT mtau
        taus' <- mapM inferExp es
        when (taus' /= taus) $ do
            faildoc $ text "Type mis-match in kernel call" <+>
                      ppr e
        return mtau

    go (UnopE op e) = do
        tau <- inferExp e
        inferUnop op tau

    go (BinopE op e1 e2) = do
       tau1 <- inferExp e1
       tau2 <- inferExp e2
       inferBinop op tau1 tau2

    go e@(IfThenElseE teste thene elsee) = do
        tau_test <- inferExp teste
        when (tau_test /= boolT) $ do
            faildoc $ text "Expected boolean type but got" <+> ppr tau_test <+>
                      text "in" <+> ppr e
        tau_thene <- inferExp thene
        tau_elsee <- inferExp elsee
        when (tau_thene /= tau_elsee) $ do
            faildoc $ text "Type mismatch in if-then-else" <+>
                      ppr tau_thene <+> text "/=" <+> ppr tau_elsee
        return tau_thene

    go (SwitchE e cases dflt) = do
        tau_e <- inferExp e
        when (not (isIntT tau_e)) $
            faildoc $ text "Scrutinee" <+> (squotes . ppr) e <+>
                      text "is not an intergral type"
        (tau:taus) <- mapM (\(_, e) -> inferExp e) cases
        mapM_ (checkBranch tau) taus
        case dflt of
          Nothing -> return ()
          Just e  -> inferExp e >>= checkBranch tau
        return tau
     where
       checkBranch :: Type -> Type -> m ()
       checkBranch tau1 tau2 | tau1 /= tau2 =
           faildoc $ text "Branch of switch has type" <+> ppr tau2 <+>
                     text "but expected" <+> ppr tau1

       checkBranch _ _ =
           return ()

    go (ReturnE e) =
        MT <$> inferExp e

    go (SeqE p1 p2) = do
        void $ inferExp p1 >>= checkMT
        tau <- inferExp p2
        void $ checkMT tau
        return tau

    go (ParE p1 p2) = do
        void $ inferExp p1 >>= checkMT
        tau <- inferExp p2
        void $ checkMT tau
        return tau

    go (BindE v tau p1 p2) = do
        tau' <- inferExp p1 >>= checkMT
        when (tau' /= tau) $
            faildoc $ text "Type mis-match in binding of" <+> ppr v
        extendVarTypes [(v, tau)] $ inferExp p2

    go (AllocE atau sh) = do
        (_, n) <- checkArrayT atau
        mapM_ (inferExp >=> checkIntT) sh
        when (length sh /= n) $
            faildoc $ text "Type mismatch in array allocation." <+>
                      text "Shape has" <+> ppr (length sh) <+> text "dimensions," <+>
                      text "but array type has" <+> ppr n
        return (MT atau)

    go (DimE _ n e) = do
        tau <- inferExp e
        case tau of
          ArrayT _ n' | n == n' ->
              return ixT
          _ ->
              faildoc $ text "Expected array type with" <+> ppr n <+>
                        text "dimentions but got" <+> ppr tau

    go (ProjArrE i n e) = do
        tau <- inferExp e
        case tau of
          ArrayT (TupleT taus) sz | length taus == n ->
              return $ ArrayT (taus !! i) sz
          _ ->
              faildoc $ text "Expected array type with" <+> ppr n <+>
                        text "components but got" <+> ppr tau

    go (IndexE v idx) = do
       (tau, _) <- inferExp v >>= checkArrayT
       inferExp idx >>= checkIntT
       return $ ScalarT tau

    go (WriteE v idx x) = do
        (tau, _) <- inferExp v >>= checkArrayT
        inferExp idx >>= checkIntT
        tau' <- inferExp x
        checkEqT tau' (ScalarT tau)
        return (MT unitT)

    go (IterateE n f x) = do
        inferExp n >>= checkIntT
        tau_x <- inferExp x
        tau_f <- inferExp f
        checkEqT tau_f (FunT [tau_x] tau_x)
        return tau_x

    go (IterateWhileE n f x) = do
        inferExp n >>= checkIntT
        tau_x <- inferExp x >>= checkScalarT
        tau_f <- inferExp f
        checkEqT tau_f (FunT [ScalarT tau_x] (ScalarT (TupleT [BoolT, tau_x])))
        return (ScalarT tau_x)

    go (ForE _ loopvs prog) = do
        taus <- mapM inferExp es
        extendVarTypes (vs `zip` taus) $
            checkExp prog (MT unitT)
        return (MT unitT)
      where
        (vs, es) = unzip loopvs

    go SyncE =
        return (MT unitT)

    go (DelayedE {}) =
        error "inferExp: encountered DelayedE"

checkExp :: forall m . MonadCheck m => Exp -> Type -> m ()
checkExp e tau = do
    tau' <- inferExp e
    checkEqT tau tau'

-- Traversal that records the types of binders

checkTraverseFam :: MonadCheck m => Traversal AST m -> Traversal AST m
checkTraverseFam trav ExpA (LetE v tau occ e1 e2) =
    LetE <$> trav VarA v
         <*> trav TypeA tau
         <*> pure occ
         <*> trav ExpA e1
         <*> extendVarTypes [(v, tau)] (trav ExpA e2)

checkTraverseFam trav ExpA (LamE vtaus e) =
    LamE vtaus <$> extendVarTypes vtaus (trav ExpA e)

checkTraverseFam trav ExpA (BindE v tau p1 p2) =
    BindE v tau <$> trav ExpA p1
                <*> extendVarTypes [(v, tau)] (trav ExpA p2)

checkTraverseFam trav ExpA (ForE isPar loopvs p) =
    ForE isPar <$> (zip vs <$> traverse (trav ExpA) es)
               <*> extendVarTypes (vs `zip` repeat ixT) (trav ExpA p)
  where
    (vs, es) = unzip loopvs

checkTraverseFam trav w a =
    traverseFam trav w a

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
    MonadCheck(..),

    inferExp,

    isIntT,
    isFloatT,
    isNumT,

    checkScalarT,
    checkArrayT,
    checkFunT,
    checkMT,

    checkExp,
    inferProgH,
    inferProcH,
    inferProgK,
    inferProcK,

    checkTraverseFam
  ) where

import Prelude hiding (mapM)

import Control.Applicative (Applicative, (<$>), (<*>), pure)
import Control.Monad ((>=>),
                      when,
                      zipWithM_)
import Control.Monad.Trans (MonadIO(..))
import Data.Traversable
import Text.PrettyPrint.Mainland

import Data.Array.Nikola.Language.Generic
import Data.Array.Nikola.Language.Syntax

class (Functor m, Applicative m, Monad m, MonadIO m) => MonadCheck m where
    lookupVarType  :: Var -> m Type
    extendVarTypes :: [(Var, Type)] -> m a -> m a

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

    go (ToFloatI FloatT)  tau | isIntT tau = return (ScalarT FloatT)
    go (ToFloatI DoubleT) tau | isIntT tau = return (ScalarT DoubleT)

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
    go DivN tau1 tau2     = joinNumT tau1 tau2

    go AndB tau1 tau2     = joinIntT tau1 tau2
    go OrB  tau1 tau2     = joinIntT tau1 tau2

    go ModI tau1 tau2     = joinIntT tau1 tau2

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

    go (ProjArrE i n e) = do
        tau <- inferExp e
        case tau of
          ArrayT (TupleT taus) sz | length taus == n ->
              return $ ArrayT (taus !! i) sz
          _ ->
              faildoc $ text "Expected array type with" <+> ppr n <+>
                        text "components but got" <+> ppr tau

    go (DimE _ n e) = do
        tau <- inferExp e
        case tau of
          ArrayT _ n' | n == n' ->
              return ixT
          _ ->
              faildoc $ text "Expected array type with" <+> ppr n <+>
                        text "dimentions but got" <+> ppr tau

    go (LetE v tau _ e1 e2) = do
        tau' <- go e1
        when (tau' /= tau) $
            faildoc $ text "Type mis-match in let binding of" <+> ppr v
        extendVarTypes [(v, tau)] $ go e2

    go (LamE vtaus e) = do
        tau  <- extendVarTypes vtaus $
                go e
        return $ FunT (map snd vtaus) tau

    go e@(AppE f es) = do
        (taus, tau) <- go f >>= checkFunT
        taus'       <- mapM go es
        when (taus' /= taus) $ do
            faildoc $ text "Type mis-match in function call" <+>
                      ppr e
        return tau

    go (UnopE op e) = do
        tau <- go e
        inferUnop op tau

    go (BinopE op e1 e2) = do
       tau1 <- go e1
       tau2 <- go e2
       inferBinop op tau1 tau2

    go e@(IfThenElseE teste thene elsee) = do
        tau_test <- go teste
        when (tau_test /= boolT) $ do
            faildoc $ text "Expected boolean type but got" <+> ppr tau_test <+>
                      text "in" <+> ppr e
        tau_thene <- go thene
        tau_elsee <- go elsee
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



    go (IndexE v idx) = do
       (tau, _) <- inferExp v >>= checkArrayT
       inferExp idx >>= checkIntT
       return $ ScalarT tau

    go (DelayedE {}) =
        error "inferExp: encountered DelayedE"

checkExp :: forall m . MonadCheck m => Exp -> Type -> m ()
checkExp e tau = do
    tau' <- inferExp e
    checkEqT tau tau'

inferProgH :: forall m . MonadCheck m => ProgH -> m Type
inferProgH = go
  where
    go :: ProgH -> m Type
    go (ReturnH e)  = inferExp e

    go (SeqH p1 p2) = go p1 >> go p2

    go (LetH v tau e p) = do
        tau' <- inferExp e
        when (tau' /= tau) $
            faildoc $ text "Type mis-match in let binding of" <+> ppr v
        extendVarTypes [(v, tau)] $ go p

    go (BindH v tau p1 p2) = do
        tau' <- inferProgH p1 >>= checkMT
        when (tau' /= tau) $
            faildoc $ text "Type mis-match in binding of" <+> ppr v
        extendVarTypes [(v, tau)] $ go p2

    go (LiftH kproc args) = do
        (taus, mtau) <- inferProcK kproc >>= checkFunT
        tau          <- checkMT mtau
        taus'        <- mapM inferExp args
        when (length taus' /= length taus) $ do
            faildoc $ text "Kernel proc" <+> ppr kproc <+>
                      text "expected" <+> ppr (length taus) <+>
                      text "arguments bu got" <+> ppr (length taus')
        zipWithM_ checkEqT taus' taus
        return (MT tau)

    go (IfThenElseH teste thene elsee) = do
        tau_test <- inferExp teste
        when (tau_test /= boolT) $ do
            faildoc $ text "Expected boolean type but got" <+> ppr tau_test
        tau_thene <- go thene
        tau_elsee <- go elsee
        when (tau_thene /= tau_elsee) $ do
            faildoc $ text "Type mismatch in if-then-else" <+>
                      ppr tau_thene <+> text "/=" <+> ppr tau_elsee
        return tau_thene

    go (AllocH atau sh) = do
        (_, n) <- checkArrayT atau
        mapM_ (inferExp >=> checkIntT) sh
        when (length sh /= n) $
            faildoc $ text "Type mismatch in array allocation." <+>
                      text "Shape has" <+> ppr (length sh) <+> text "dimensions," <+>
                      text "but array type has" <+> ppr n
        return (MT atau)

    go (DelayedH {}) =
        error "inferProgH: encountered DelayedH"

inferProcH :: MonadCheck m => ProcH -> m Type
inferProcH (ProcH vtaus prog) = do
    tau <- extendVarTypes vtaus $ inferProgH prog
    return $ FunT (map snd vtaus) tau

inferProgK :: forall m . MonadCheck m => ProgK -> m Type
inferProgK = go
  where
    go :: ProgK -> m Type
    go (ReturnK e)  = MT <$> inferExp e
    go (SeqK p1 p2) = go p1 >> go p2
    go (ParK p1 p2) = go p1 >> go p2

    go (LetK v tau e p) = do
        tau' <- inferExp e
        when (tau' /= tau) $
            faildoc $ text "Type mis-match in let binding of" <+> ppr v
        extendVarTypes [(v, tau)] $ go p

    go (BindK v tau p1 p2) = do
        tau' <- inferProgK p1 >>= checkMT
        when (tau' /= tau) $
            faildoc $ text "Type mis-match in binding of" <+> ppr v
        extendVarTypes [(v, tau)] $ go p2

    go (ForK vs es prog) = do
        taus <- mapM inferExp es
        extendVarTypes (vs `zip` taus) $
            checkProgK prog unitT
        return (MT unitT)

    go (ParforK vs es prog) = do
        taus <- mapM inferExp es
        extendVarTypes (vs `zip` taus) $ do
        inferProgK prog

    go (IfThenElseK teste thene elsee) = do
        tau_test <- inferExp teste
        when (tau_test /= boolT) $ do
            faildoc $ text "Expected boolean type but got" <+> ppr tau_test
        tau_thene <- go thene
        tau_elsee <- go elsee
        when (tau_thene /= tau_elsee) $ do
            faildoc $ text "Type mismatch in if-then-else" <+>
                      ppr tau_thene <+> text "/=" <+> ppr tau_elsee
        return tau_thene

    go (WriteK v idx x) = do
        (tau, _) <- inferExp v >>= checkArrayT
        inferExp idx >>= checkIntT
        tau' <- inferExp x
        checkEqT tau' (ScalarT tau)
        return (MT unitT)

    go SyncK =
        return (MT unitT)

    go (DelayedK {}) =
        error "inferProgK: encountered DelayedK"

checkProgK :: forall m . MonadCheck m => ProgK -> Type -> m ()
checkProgK prog tau = do
    tau' <- inferProgK prog
    checkEqT tau tau'

inferProcK :: MonadCheck m => ProcK -> m Type
inferProcK (ProcK vtaus prog) = do
    tau <- extendVarTypes vtaus $ inferProgK prog
    return $ FunT (map snd vtaus) tau

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

checkTraverseFam trav ProgHA (BindH v tau p1 p2) =
    BindH v tau <$> trav ProgHA p1
                <*> extendVarTypes [(v, tau)] (trav ProgHA p2)

checkTraverseFam trav ProcHA (ProcH vtaus p) =
    ProcH vtaus <$> extendVarTypes vtaus (trav ProgHA p)

checkTraverseFam trav ProgKA (BindK v tau p1 p2) =
    BindK v tau <$>  trav ProgKA p1
                <*> extendVarTypes [(v, tau)] (trav ProgKA p2)

checkTraverseFam trav ProgKA (ForK vs es p) =
    ForK vs <$> traverse (trav ExpA) es
            <*> extendVarTypes (vs `zip` repeat ixT) (trav ProgKA p)

checkTraverseFam trav ProgKA (ParforK vs es p) =
    ParforK vs <$> traverse (trav ExpA) es
               <*> extendVarTypes (vs `zip` repeat ixT) (trav ProgKA p)

checkTraverseFam trav ProcKA (ProcK vtaus p) =
    ProcK vtaus <$> extendVarTypes vtaus (trav ProgKA p)

checkTraverseFam trav w a =
    traverseFam trav w a

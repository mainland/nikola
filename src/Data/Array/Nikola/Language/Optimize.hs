{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Data.Array.Nikola.Language.Optimize
-- Copyright   : (c) Geoffrey Mainland 2012
-- License     : BSD-style
--
-- Maintainer  : Geoffrey Mainland <mainland@apeiron.net>
-- Stability   : experimental
-- Portability : non-portable

module Data.Array.Nikola.Language.Optimize where

import Prelude hiding (mapM)

import Control.Applicative (Applicative, (<$>), (<*>), pure)
import Control.Monad ((>=>))
import Control.Monad.State (StateT(..), evalStateT,
                            MonadState(..), gets, modify)
import Control.Monad.Trans (MonadIO(..))
import Data.Foldable
import Data.Int
import Data.Monoid
import Data.List (foldl1')
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Traversable
import Text.PrettyPrint.Mainland

import Data.Array.Nikola.Backend.Flags
import Data.Array.Nikola.Exp hiding (Var, Exp)
import qualified Data.Array.Nikola.Exp as E
import Data.Array.Nikola.Program

import Data.Array.Nikola.Language.Check
import Data.Array.Nikola.Language.Generic
import Data.Array.Nikola.Language.Monad
import Data.Array.Nikola.Language.Syntax

-- Host program optimization. Some passes rely on normalized monadic structure,
-- so we perform an initial normalization pass and then do another final
-- normalization pass after other optimizations have had a chance to run.

optimizeHostProgram :: ProcH -> R r ProcH
optimizeHostProgram =
    return
    >=> oPass (norm ProcHA)
    -- >=> whenDialect CUDA (oPass (mergeParfor ProcHA))
    >=> whenDialect CUDA (splitKernels ProcHA)
    >=> oPass (shareBindings ProcHA)
    >=> aPass (mergeBounds ProcHA)
    >=> oPass (norm ProcHA)
    >=> oPass (occ ProcHA)
    >=> oPass (simpl ProcHA)
    >=> oPass (norm ProcHA)
    >=> oPass (lambdaLift ProcHA)

liftHostProgram :: ProcH -> R r ProcH
liftHostProgram =
    return
    >=> oPass (lambdaLift ProcHA)

whenDialect :: Dialect
            -> (ProcH -> R r ProcH)
            -> ProcH
            -> R r ProcH
whenDialect dialect f p = do
    flags <- getFlags
    if (fromLJust fDialect flags == dialect)
      then f p
      else return p

oPass :: (ProcH -> O ProcH) -> ProcH -> R r ProcH
oPass f = liftIO . evalO . f

aPass :: (ProcH -> A ProcH) -> ProcH -> R r ProcH
aPass f = liftIO . evalA . f

-- Helpers

ifK :: E.Exp t a -> ProgK -> ProgK
ifK e p = IfThenElseK (unE e) p (ReturnK UnitE)

-- The optimization monad
data OEnv = OEnv
    { oVarVarSubst :: Map Var Var
    , oVarExpSubst :: Map Var Exp
    , oVarTypes    :: Map Var Type
    , oOcc         :: Map Var Occ
    }

defaultOEnv :: OEnv
defaultOEnv = OEnv { oVarVarSubst = Map.empty
                   , oVarExpSubst = Map.empty
                   , oVarTypes    = Map.empty
                   , oOcc         = Map.empty
                   }

newtype O a = O { unO :: StateT OEnv IO a }
  deriving (Monad, Functor, Applicative, MonadState OEnv, MonadIO)

evalO :: O a -> IO a
evalO m = evalStateT (unO m) defaultOEnv

runO :: O a -> IO (a, OEnv)
runO m = runStateT (unO m) defaultOEnv

instance MonadSubst Var Var O where
    getTheta _ _              = gets   $ \s -> Theta (oVarVarSubst s) Set.empty
    putTheta (Theta theta' _) = modify $ \s -> s { oVarVarSubst = theta' }

instance MonadSubst Var Exp O where
    getTheta _ _              = gets   $ \s -> Theta (oVarExpSubst s) Set.empty
    putTheta (Theta theta' _) = modify $ \s -> s { oVarExpSubst = theta' }

instance MonadCheck O where
    lookupVarType v = do
        maybe_tau <- gets $ \s -> Map.lookup v (oVarTypes s)
        case maybe_tau of
          Just tau -> return tau
          Nothing ->  faildoc $ text "Variable" <+> ppr v <+>
                                text "not in scope during reification."

    extendVarTypes vtaus act = do
        old_vars <- gets oVarTypes
        modify $ \s -> s { oVarTypes = foldl' insert (oVarTypes s) vtaus }
        x  <- act
        modify $ \s -> s { oVarTypes = old_vars }
        return x
      where
        insert m (k, v) = Map.insert k v m

occVar :: Var -> O ()
occVar v =
    modify $ \s -> s { oOcc = Map.alter alter v (oOcc s) }
  where
    alter :: Maybe Occ -> Maybe Occ
    alter Nothing    = Just Once
    alter (Just occ) = Just $ occ `occJoin` Once

occsJoin :: Map Var Occ -> Map Var Occ -> Map Var Occ
occsJoin occs1 occs2 = Map.unionWith occJoin occs1 occs2

occsMeet :: Map Var Occ -> Map Var Occ -> Map Var Occ
occsMeet occs1 occs2 = Map.unionWith occMeet occs1 occs2

occsDelete :: [Var] -> Map Var Occ -> Map Var Occ
occsDelete vs occ = foldl' (flip Map.delete) occ vs

withOcc :: O a -> O (a, Map Var Occ)
withOcc act = do
    old_occ <- gets oOcc
    modify $ \s -> s { oOcc = Map.empty }
    a   <- act
    occ <- gets oOcc
    modify $ \s -> s { oOcc = old_occ }
    return (a, occ)

setOcc :: Map Var Occ -> O ()
setOcc occ =
    modify $ \s -> s { oOcc = occ }

-- Deciding whether or not things must be equal ("must be equal" implies
-- "equal", but "equal" does not imply "must be equal")

infix 4 ==!

class MustEq a where
    (==!) :: a -> a -> Bool

instance MustEq a => MustEq [a] where
    []     ==! []     = True
    (x:xs) ==! (y:ys) = x ==! y && xs ==! ys
    _      ==! _      = False

instance MustEq Exp where
    ConstE c1               ==! ConstE c2               = c1 == c2
    VarE v1                 ==! VarE v2                 = v1 == v2
    UnitE                   ==! UnitE                   = True
    DimE i1 n1 e1           ==! DimE i2 n2 e2           = i1 == i2 && n1 == n2 && e1 == e2
    LetE v1 tau1 _ e1a e1b  ==! LetE v2 tau2 _ e2a e2b  = v1 == v2 && tau1 == tau2 && e1a ==! e2a && e1b ==! e2b
    LamE vtaus1 e1          ==! LamE vtaus2 e2          = vtaus1 == vtaus2 && e1 ==! e2
    AppE e1 es1             ==! AppE e2 es2             = e1 ==! e2 && es1 ==! es2
    UnopE op1 e1            ==! UnopE op2 e2            = op1 == op2 && e1 ==! e2
    BinopE op1 e1a e1b      ==! BinopE op2 e2a e2b      = op1 == op2 && e1a ==! e2a && e1b ==! e2b
    IfThenElseE e1a e1b e1c ==! IfThenElseE e2a e2b e2c = e1a ==! e2a && e1b ==! e2b && e1c ==! e2c
    IndexE v1 idx1          ==! IndexE v2 idx2          = v1 == v2 && idx1 ==! idx2
    _                       ==! _                       = False

occ :: AST a -> a -> O a
occ ExpA (VarE v) = do  v' <- occ VarA v
                        occVar v'
                        return $ VarE v'

occ ExpA (BinopE MaxO e1 e2) = do
    (e1', occ1) <- withOcc $ occ ExpA e1
    (e2', occ2) <- withOcc $ occ ExpA e2
    setOcc $ Map.map (const Many) (occ1 `Map.union` occ2)
    return $ BinopE MaxO e1' e2'

occ ExpA (BinopE MinO e1 e2) = do
    (e1', occ1) <- withOcc $ occ ExpA e1
    (e2', occ2) <- withOcc $ occ ExpA e2
    setOcc $ Map.map (const Many) (occ1 `Map.union` occ2)
    return $ BinopE MinO e1' e2'

occ ExpA (LetE v tau _ e1 e2) = do
    (e1', occ1) <- withOcc $ occ ExpA e1
    (e2', occ2) <- withOcc $ occ ExpA e2
    let occ = Map.findWithDefault Never v occ2
    setOcc $ occsDelete [v] occ1 `occsJoin` occ2
    return $ LetE v tau occ e1' e2'

occ ExpA (LamE vtaus e) = do
    (e', occ) <- withOcc $ occ ExpA e
    setOcc $ occsDelete (map fst vtaus) occ
    return $ LamE vtaus e'

occ ExpA (IfThenElseE e1 e2 e3) = do
    (e1', occ1) <- withOcc $ occ ExpA e1
    (e2', occ2) <- withOcc $ occ ExpA e2
    (e3', occ3) <- withOcc $ occ ExpA e3
    setOcc $ occ1 `occsJoin` (occ2 `occsMeet` occ3)
    return $ IfThenElseE e1' e2' e3'

occ ProgKA (ForK vs es p) = do
    vs'         <- traverse (occ VarA) vs
    (es', occs) <- withOcc $ traverse (occ ExpA) es
    setOcc $ occsDelete vs occs
    ForK vs' es' <$> occ ProgKA p

occ ProgKA (ParforK vs es p) = do
    vs'         <- traverse (occ VarA) vs
    (es', occs) <- withOcc $ traverse (occ ExpA) es
    setOcc $ occsDelete vs occs
    ParforK vs' es' <$> occ ProgKA p

occ ProgKA (IfThenElseK e1 p1 p2) = do
    (e1', occ1) <- withOcc $ occ ExpA e1
    (p1', occ2) <- withOcc $ occ ProgKA p1
    (p2', occ3) <- withOcc $ occ ProgKA p2
    setOcc $ occ1 `occsJoin` (occ2 `occsMeet` occ3)
    return $ IfThenElseK e1' p1' p2'

occ ProgKA (WriteK v idx e) = WriteK <$> occ ExpA v
                                     <*> occ ExpA idx
                                     <*> occ ExpA e

occ ProgHA (IfThenElseH e1 p1 p2) = do
    (e1', occ1) <- withOcc $ occ ExpA e1
    (p1', occ2) <- withOcc $ occ ProgHA p1
    (p2', occ3) <- withOcc $ occ ProgHA p2
    setOcc $ occ1 `occsJoin` (occ2 `occsMeet` occ3)
    return $ IfThenElseH e1' p1' p2'

occ w a = traverseFam occ w a

-- Kernel splitting

splitKernels :: AST a -> a -> R r a
splitKernels = split
  where
    split :: AST a -> a -> R r a
    split ProgHA (LiftH (ProcK [] p) []) = do
        resetH $ splitK ProgKA p

    split w a = checkTraverseFam split w a

    splitK :: AST a -> a -> R ProgK a
    -- Sequential parallel fors must be split into separate kernels.
    splitK ProgKA (SeqK m1@(ParforK {}) m2) = do
        m1' <- checkTraverseFam splitK ProgKA m1
        m2' <- reset $ checkTraverseFam splitK ProgKA m2
        SeqK m1' <$> isolateK m2'

    splitK w a = checkTraverseFam splitK w a

-- Simplification folding
isAtomic :: Exp -> Bool
isAtomic (VarE {})        = True
isAtomic (ConstE {})      = True
isAtomic (ProjArrE _ _ e) = isAtomic e
isAtomic (DimE _ _ e)     = isAtomic e
isAtomic (UnopE NegN _)   = True
isAtomic _                = False

simpl :: AST a -> a -> O a
simpl ExpA (VarE v) = lookupSubst VarA v ExpA (return (VarE v))

simpl ExpA (LetE _ _ Never _ e2) =
    simpl ExpA e2

simpl ExpA (LetE v _ Once e1 e2) = do
    e1' <- simpl ExpA e1
    insertSubst VarA v ExpA e1'
    simpl ExpA e2

simpl ExpA (LetE v tau Many e1 e2) =
    LetE v tau Many <$> simpl ExpA e1 <*> simpl ExpA e2

simpl ExpA (UnopE op e) = do  e' <- simpl ExpA e
                              go op e'
  where
    go :: Unop -> Exp -> O Exp
    go SqrtF  (ConstE (FloatC i)) = pure $ ConstE (FloatC (sqrt i))
    go RecipF (ConstE (FloatC i)) = pure $ ConstE (FloatC (1/i))

    go SqrtF  (ConstE (DoubleC i)) = pure $ ConstE (DoubleC (sqrt i))
    go RecipF (ConstE (DoubleC i)) = pure $ ConstE (DoubleC (1/i))

    go op e = return $ UnopE op e

simpl ExpA (BinopE op e1 e2) = do  e1' <- simpl ExpA e1
                                   e2' <- simpl ExpA e2
                                   go op e1' e2'
  where
    go :: Binop -> Exp -> Exp -> O Exp
    go AddN (ConstE (Int32C 0))  e2                   = pure e2
    go AddN (ConstE (FloatC 0))  e2                   = pure e2
    go AddN (ConstE (DoubleC 0)) e2                   = pure e2
    go AddN (ConstE (Int32C i))  (ConstE (Int32C j))  = pure $ ConstE (Int32C (i+j))
    go AddN (ConstE (FloatC i))  (ConstE (FloatC j))  = pure $ ConstE (FloatC (i+j))
    go AddN (ConstE (DoubleC i)) (ConstE (DoubleC j)) = pure $ ConstE (DoubleC (i+j))
    go AddN e1                   (BinopE AddN e2 e3)  = simpl ExpA (BinopE AddN (BinopE AddN e1 e2) e3)
    go AddN e1                   e2@(ConstE {})       = simpl ExpA (BinopE AddN e2 e1)

    go SubN e1                  (ConstE (Int32C i))   = simpl ExpA (BinopE AddN (ConstE (Int32C (negate i))) e1)
    go SubN e1                  (ConstE (FloatC i))   = simpl ExpA (BinopE AddN (ConstE (FloatC (negate i))) e1)
    go SubN e1                  (ConstE (DoubleC i))  = simpl ExpA (BinopE AddN (ConstE (DoubleC (negate i))) e1)

    go MulN (ConstE (Int32C 0))  _                    = pure $ ConstE (Int32C 0)
    go MulN (ConstE (FloatC 0))  _                    = pure $ ConstE (FloatC 0)
    go MulN (ConstE (DoubleC 0)) _                    = pure $ ConstE (DoubleC 0)
    go MulN (ConstE (Int32C 1))  e2                   = pure e2
    go MulN (ConstE (FloatC 1))  e2                   = pure e2
    go MulN (ConstE (DoubleC 1)) e2                   = pure e2
    go MulN (ConstE (Int32C i))  (ConstE (Int32C j))  = pure $ ConstE (Int32C (i*j))
    go MulN (ConstE (FloatC i))  (ConstE (FloatC j))  = pure $ ConstE (FloatC (i*j))
    go MulN (ConstE (DoubleC i)) (ConstE (DoubleC j)) = pure $ ConstE (DoubleC (i*j))
    go MulN e1                   e2@(ConstE {})       = simpl ExpA (BinopE MulN e2 e1)
    go MulN e1                   (BinopE MulN e2 e3)  = simpl ExpA (BinopE MulN (BinopE MulN e1 e2) e3)

    go ModI e1                  (ConstE (Int32C 1))   = pure e1

    go DivN (ConstE (FloatC 1)) e2                    = simpl ExpA (UnopE RecipF e2)
    go DivN (ConstE (DoubleC 1)) e2                   = simpl ExpA (UnopE RecipF e2)

    -- Default
    go op e1 e2 = return $ BinopE op e1 e2

simpl ProgHA (LiftH (ProcK [] (SeqK m1 (ReturnK e))) []) =
    simpl ProgHA (SeqH (LiftH (ProcK [] m1) []) (ReturnH e))

simpl w a = traverseFam simpl w a

-- Lift shared bindings out of branches

shareBindings :: forall a m . (MonadSubst Var Var m, MonadSubst Var Exp m) => AST a -> a -> m a
shareBindings VarA v        = lookupSubst VarA v VarA (return v)
shareBindings ExpA (VarE v) = lookupSubst VarA v ExpA (VarE <$> shareBindings VarA v)

shareBindings ExpA (IfThenElseE test (LetE v1 tau1 occ1 e1a e1b) (LetE v2 tau2 occ2 e2a e2b)) = do
    e1a' <- shareBindings ExpA e1a
    e2a' <- shareBindings ExpA e2a
    if e1a' == e2a'
       then do  insertSubst VarA v2 VarA v1
                LetE v1 tau1 (occJoin occ1 occ2) e1a' <$> shareBindings ExpA (IfThenElseE test e1b e2b)
       else do  e1 <- LetE <$> shareBindings VarA v1
                           <*> shareBindings TypeA tau1
                           <*> pure occ1
                           <*> pure e1a'
                           <*> shareBindings ExpA e1b
                e2 <- LetE <$> shareBindings VarA v2
                           <*> shareBindings TypeA tau2
                           <*> pure occ2
                           <*> pure e2a'
                           <*> shareBindings ExpA e2b
                IfThenElseE <$> shareBindings ExpA test <*> pure e1 <*> pure e2

shareBindings w a = traverseFam shareBindings w a

-- Merge parallel for loops

mergeParfor :: forall a m . (MonadSubst Var Var m, MonadSubst Var Exp m) => AST a -> a -> m a
mergeParfor VarA v        = lookupSubst VarA v VarA (return v)
mergeParfor ExpA (VarE v) = lookupSubst VarA v ExpA (VarE <$> mergeParfor VarA v)

mergeParfor ProcKA (ProcK vtaus p) = do
    ProcK vtaus <$> (fromMnfK <$> go (toMnfK p))
  where
    go :: Mnf ProgK -> m (Mnf ProgK)
    go [] =
        return []

    go ((seq1, ParforK [v1] [e1] p1) : (seq2, ParforK [v2] [e2] p2) : ms) = do
        insertSubst VarA v2 VarA v1
        let p1' = ifK ((E . VarE) v1 <* (E e1 :: E.Exp t Int32)) p1
        let p2' = ifK ((E . VarE) v1 <* (E e2 :: E.Exp t Int32)) p2
        go ((seq1, ParforK [v1] [BinopE MaxO e1 e2] (sync p1' p2')) : ms)
      where
        sync = case seq2 of
                 SeqM -> \m1 m2 -> m1 `seqK` SyncK `seqK` m2
                 ParM -> seqK
                 _    -> error "mergeParfor: saw bind between parallel fors"

    go ((s,m) : ms) = do
        m'  <- mergeParfor ProgKA m
        ms' <- go ms
        return $ (s,m') : ms'

mergeParfor w      a    = traverseFam mergeParfor w a

-- Convert monadic actions to a normal form
data SeqM = SeqM
          | ParM
          | BindM Var Type

instance Pretty SeqM where
    ppr SeqM          = text ">>"
    ppr ParM          = text "||"
    ppr (BindM v tau) = text ">>=" <+> text "\\" <+> ppr v <+> text "::" <+> ppr tau <+> text "->"

type Mnf a = [(SeqM, a)]

instance Pretty a => Pretty (Mnf a) where
    ppr [] = empty
    ppr ((s, m):ms) = ppr m </> ppr s </> ppr ms

toMnfK :: ProgK -> Mnf ProgK
toMnfK (SeqK (ReturnK {}) m)         = toMnfK m
toMnfK (SeqK (SeqK m1 m2) m3)        = toMnfK (SeqK m1 (SeqK m2 m3))
toMnfK (SeqK (ParK m1 m2) m3)        = toMnfK (SeqK m1 (ParK m2 m3))
toMnfK (SeqK (BindK v tau m1 m2) m3) = toMnfK (BindK v tau m1 (SeqK m2 m3))
toMnfK (SeqK m1 m2)                  = (SeqM, m1) : toMnfK m2

toMnfK (ParK (ReturnK {}) m)         = toMnfK m
toMnfK (ParK (SeqK m1 m2) m3)        = toMnfK (ParK m1 (SeqK m2 m3))
toMnfK (ParK (ParK m1 m2) m3)        = toMnfK (ParK m1 (ParK m2 m3))
toMnfK (ParK (BindK v tau m1 m2) m3) = toMnfK (BindK v tau m1 (ParK m2 m3))
toMnfK (ParK m1 m2)                  = (ParM, m1) : toMnfK m2

toMnfK (BindK v tau (SeqK m1 m2) m3) = toMnfK (SeqK m1 (BindK v tau m2 m3))
toMnfK (BindK v tau (ParK m1 m2) m3) = toMnfK (ParK m1 (BindK v tau m2 m3))
toMnfK (BindK v2 tau2
          (BindK v1 tau1 m1 m2) m3)  = toMnfK (BindK v1 tau1 m1
                                                 (BindK v2 tau2 m2 m3))
toMnfK (BindK v tau m1 m2)           = (BindM v tau, m1) : toMnfK m2

toMnfK m                             = [(SeqM, m)]

fromMnfK :: Mnf ProgK -> ProgK
fromMnfK []              = error "fromMnfK: empty list"
fromMnfK [(SeqM, m)]     = m
fromMnfK [(ParM, _)]     = error "fromMnfK: last action is a par"
fromMnfK [(BindM {}, _)] = error "fromMnfK: last action is a bind"

fromMnfK ((SeqM, m)       :ms) = SeqK m $ fromMnfK ms
fromMnfK ((ParM, m)       :ms) = ParK m $ fromMnfK ms
fromMnfK ((BindM v tau, m):ms) = BindK v tau m $ fromMnfK ms

-- Normalize monadic actions

norm :: (MonadCheck m, MonadSubst Var Var m, MonadSubst Var Exp m) => AST a -> a -> m a
norm VarA v        = lookupSubst VarA v VarA (return v)
norm ExpA (VarE v) = lookupSubst VarA v ExpA (VarE <$> norm VarA v)

norm ProgHA (SeqH (ReturnH {}) m)                    = norm ProgHA m
norm ProgHA (SeqH (SeqH m1 m2) m3)                   = norm ProgHA (SeqH m1 (SeqH m2 m3))
norm ProgHA (SeqH (BindH v tau m1 m2) m3)            = norm ProgHA (BindH v tau m1 (SeqH m2 m3))
norm ProgHA (SeqH m1 (ReturnH UnitE))                = do  m1' <- norm ProgHA m1
                                                           tau <- inferProgH m1' >>= checkMT
                                                           if tau == unitT
                                                             then norm ProgHA m1
                                                             else norm ProgHA $ SeqH m1' (ReturnH UnitE)
norm ProgHA (BindH v _ (ReturnH e) m)                = do  insertSubst VarA v ExpA e
                                                           norm ProgHA m
norm ProgHA (BindH v tau (SeqH m1 m2) m3)            = norm ProgHA (SeqH m1 (BindH v tau m2 m3))
norm ProgHA (BindH v2 tau2 (BindH v1 tau1 m1 m2) m3) = norm ProgHA (BindH v1 tau1 m1 (BindH v2 tau2 m2 m3))
norm ProgHA (LiftH (ProcK [] (ReturnK e)) [])        = do  m <- ReturnH <$> norm ExpA e
                                                           norm ProgHA m

norm ProgKA (SeqK (ReturnK {}) m)                    = norm ProgKA m
norm ProgKA (SeqK (SeqK m1 m2) m3)                   = norm ProgKA (SeqK m1 (SeqK m2 m3))
norm ProgKA (ParK (ParK m1 m2) m3)                   = norm ProgKA (ParK m1 (ParK m2 m3))
norm ProgKA (SeqK (BindK v tau m1 m2) m3)            = norm ProgKA (BindK v tau m1 (SeqK m2 m3))
norm ProgKA (SeqK m1 (ReturnK UnitE))                = do  m1' <- norm ProgKA m1
                                                           tau <- inferProgK m1' >>= checkMT
                                                           if tau == unitT
                                                             then norm ProgKA m1
                                                             else norm ProgKA $ SeqK m1' (ReturnK UnitE)
norm ProgKA (BindK v tau (SeqK m1 m2) m3)            = norm ProgKA (SeqK m1 (BindK v tau m2 m3))
norm ProgKA (BindK v2 tau2 (BindK v1 tau1 m1 m2) m3) = norm ProgKA (BindK v1 tau1 m1 (BindK v2 tau2 m2 m3))

norm w a = checkTraverseFam norm w a

-- Lambda-lift kernels

lambdaLift :: forall m a . MonadCheck m => AST a -> a -> m a
lambdaLift ProgHA (LiftH (ProcK vtaus m) es) = do  let vs' =  Set.toList (fvs ProgKA m)
                                                   taus'   <- mapM lookupVarType vs'
                                                   return $ LiftH (ProcK (vtaus ++ vs' `zip` taus') m) (es ++ map VarE vs')
lambdaLift w      a                          = checkTraverseFam lambdaLift w a

-- Free variables
fvs :: AST a -> a -> Set Var
fvs w a = fst (vars w a Set.empty)

vars :: Fold AST (Set Var -> (Set Var, Set Var))
vars = go
  where
    go :: Fold AST (Set Var -> (Set Var, Set Var))
    go VarA   v                          = useVar v
    go ExpA   (LetE v _ _ e1 e2)         = go ExpA e1 `mappend`
                                           bindVar v (go ExpA e2)
    go ExpA   (LamE vtaus e)             = bindVars (map fst vtaus) (go ExpA e)
    go ProgKA (BindK v _ p1 p2)          = go ProgKA p1 `mappend`
                                           bindVar v (go ProgKA p2)
    go ProgKA (ForK vs es p)             = foldMap (go ExpA) es `mappend`
                                           bindVars vs (go ProgKA p)
    go ProgKA (ParforK vs es p)          = foldMap (go ExpA) es `mappend`
                                           bindVars vs (go ProgKA p)
    go ProgKA (WriteK v idx e)           = go ExpA v `mappend`
                                           go ExpA idx `mappend`
                                           go ExpA e
    go ProcKA (ProcK vtaus p)            = bindVars (map fst vtaus) (go ProgKA p)
    go ProgHA (BindH v _ p1 p2)          = go ProgHA p1 `mappend`
                                           bindVar v (go ProgHA p2)
    go ProcHA (ProcH vtaus p)            = bindVars (map fst vtaus) (go ProgHA p)
    go w      a                          = foldFam go w a

    useVar :: Var -> (Set Var -> (Set Var, Set Var))
    useVar v = \bound -> (if v `Set.member` bound then mempty else Set.singleton v, mempty)

    bindVar :: Var
            -> (Set Var -> (Set Var, Set Var))
            -> (Set Var -> (Set Var, Set Var))
    bindVar v m = \bound -> m (Set.insert v bound)

    bindVars :: [Var]
             -> (Set Var -> (Set Var, Set Var))
             -> (Set Var -> (Set Var, Set Var))
    bindVars vs m = \bound -> m (bound `Set.union` Set.fromList vs)

-- Binders
class Ord a => Binder a where
    uniqBinder :: a -> Set a -> a

class Binder a => BinderOcc a b where
    binderOcc  :: a -> b

instance Binder Var where
    uniqBinder (Var s) phi =
        head [v'  | i <- [show i | i <- [1..]]
                  , let v' = Var (s ++ "_" ++ i)
                  , v' `Set.notMember` phi]

instance BinderOcc Var Var where
    binderOcc  = id

instance BinderOcc Var Exp where
    binderOcc  = VarE

-- Substitutions
data Theta a b = Theta { theta :: Map a b, phi :: Set a }

class (Monad m, Functor m, Applicative m, BinderOcc a b) => MonadSubst a b m where
    getTheta :: AST a -> AST b -> m (Theta a b)
    putTheta :: Theta a b -> m ()

    getsTheta :: AST a -> AST b -> (Theta a b -> c) -> m c
    getsTheta wa wb f = do
        s <- getTheta wa wb
        return (f s)

    modifyTheta :: AST a -> AST b -> (Theta a b -> Theta a b) -> m ()
    modifyTheta wa wb f = do
        s <- getTheta wa wb
        putTheta (f s)

lookupSubst :: MonadSubst a b m => AST a -> a -> AST b -> m b -> m b
lookupSubst wa a wb mb = do
    maybe_b <- getsTheta wa wb (Map.lookup a . theta)
    case maybe_b of
      Nothing -> mb
      Just b' -> return b'

insertSubst :: MonadSubst a b m => AST a -> a -> AST b -> b -> m ()
insertSubst wa a wb b =
    modifyTheta wa wb $ \s -> s { theta = Map.insert a b (theta s) }

bind :: MonadSubst a b m
     => AST a
     -> AST b
     -> a
     -> (a -> m c)
     -> m c
bind wa wb v kont = do
    old_phi   <- getsTheta wa wb phi
    old_theta <- getsTheta wa wb theta
    a <- if v `Set.member` old_phi
         then do  let v' = uniqBinder v old_phi
                  modifyTheta wa wb $ \s -> s { phi   = Set.insert v' old_phi
                                              , theta = Map.insert v (binderOcc v') old_theta
                                              }
                  kont v'
         else do  modifyTheta wa wb $ \s -> s { phi = Set.insert v (phi s) }
                  kont v
    modifyTheta wa wb $ \s -> s { phi   = old_phi
                                , theta = old_theta
                                }
    return a

binds :: forall m a b c . MonadSubst a b m
      => AST a
      -> AST b
      -> [a]
      -> ([a] -> m c)
      -> m c
binds _ _ [] kont = kont []

binds wa wb (v:vs) kont = do
    bind  wa wb v  $ \v'  -> do
    binds wa wb vs $ \vs' -> do
    kont (v':vs')

-- Substitution
subst :: forall m a b . MonadSubst a b m => AST a -> AST b -> forall c . AST c -> c -> m c
subst = go
  where
    go :: AST a -> AST b -> AST c -> c -> m c
    go VarA VarA VarA v                  = lookupSubst VarA v VarA (return v)

    go VarA ExpA ExpA e@(VarE v)         = lookupSubst VarA v ExpA (return e)

    go VarA w ExpA (LetE v tau occ e1 e2) = do  e1' <- go VarA w ExpA e1
                                                bind VarA w v $ \v'-> do
                                                LetE v' tau occ  e1' <$> go VarA w ExpA e2

    go VarA w ExpA (LamE vtaus e)     = do  let (vs, taus) = unzip vtaus
                                            binds VarA w vs $ \vs' -> do
                                            LamE (vs' `zip` taus) <$> go VarA w ExpA e

    go VarA w ProgHA (BindH v tau p1 p2)  = do  p1' <- go VarA w ProgHA p1
                                                bind VarA w v $ \v' -> do
                                                BindH v' tau p1' <$> go VarA w ProgHA p2

    go VarA w ProcHA (ProcH vtaus p)  = do  let (vs, taus) = unzip vtaus
                                            binds VarA w vs $ \vs' -> do
                                            ProcH (vs' `zip` taus) <$> go VarA w ProgHA p

    go VarA w ProgKA (BindK v tau p1 p2)  = do  p1' <- go VarA w ProgKA p1
                                                bind VarA w v $ \v' -> do
                                                BindK v' tau p1' <$> go VarA w ProgKA p2

    go VarA w ProgKA (ForK vs es p)    = do  es' <- traverse (go VarA w ExpA) es
                                             binds VarA w vs $ \vs' -> do
                                             ForK vs' es' <$> go VarA w ProgKA p

    go VarA w ProgKA (ParforK vs es p) = do  es' <- traverse (go VarA w ExpA) es
                                             binds VarA w vs $ \vs' -> do
                                             ParforK vs' es' <$> go VarA w ProgKA p

    go VarA w ProgKA (WriteK v idx e)  = WriteK <$> go VarA w ExpA v
                                                <*> go VarA w ExpA idx
                                                <*> go VarA w ExpA e

    go VarA w ProcKA (ProcK vtaus p)  = do  let (vs, taus) = unzip vtaus
                                            binds VarA w vs $ \vs' -> do
                                            p' <- go VarA w ProgKA p
                                            return $ ProcK (vs' `zip` taus) p'

    go w1 w2 w a = traverseFam (go w1 w2) w a

-- The StateL monad
newtype StateL s a = StateL { runStateL :: s -> (s, a) }

instance Functor (StateL s) where
    fmap f (StateL k) = StateL $ \s ->
                        let (s', v) = k s
                        in
                          (s', f v)

instance Applicative (StateL s) where
    pure x = StateL $ \s -> (s, x)

    StateL kf <*> StateL kv = StateL $ \s ->
        let (s',   f) = kf s
            (s'',  v) = kv s'
        in (s'', f v)

instance Monad (StateL s) where
    m >>= f  = StateL $ \s -> let (s', a) = runStateL m s
                              in
                                runStateL (f a) s'

    m1 >> m2 = StateL $ \s -> let (s', _) = runStateL m1 s
                              in
                                runStateL m2 s'

    return x = StateL $ \s -> (s, x)

instance MonadState s (StateL s) where
    get   = StateL $ \s -> (s, s)
    put s = StateL $ \_ -> (s, ())

-- A substitution monad
type Subst a b = StateL (Theta a b)

instance BinderOcc a b => MonadSubst a b (Subst a b) where
    getTheta _ _ = get
    putTheta s   = put s

{-
subst1 :: (MonadCheck m, MonadState REnv m, BinderOcc Var a) => Var -> AST a -> a -> AST b -> b -> m b
subst1 v w1 e w a = do
    phi <- gets (Map.keysSet . rVarTypes)
    let (_, x) = runStateL (subst VarA w1 w a) (Theta { theta = Map.fromList [(v, e)]
                                                      , phi   = phi
                                                      })
    return x
-}

-- Abstract interpretation

data I = I (Maybe Integer) (Set Exp)
  deriving (Eq, Ord)

instance Pretty I where
    ppr (I Nothing es) =
        (parens . commasep . map ppr . Set.toList) es
    ppr (I (Just c) es) =
        (parens . commasep) (ppr c : (map ppr . Set.toList) es)

iToExp :: I -> Exp
iToExp (I (Just c) vs) =
    foldl1' (BinopE MaxO) (ConstE (Int32C (fromIntegral c)) : Set.toList vs)

iToExp (I Nothing vs) =
    foldl1' (BinopE MaxO) (Set.toList vs)

-- | A 'Range' represents a range (loop bound) [0,n). Either we know the upper
-- bound precisely, and it is of the form max(c,e1,...,en) where c is a constant
-- and e1...en are expressions, or we don't know the bound, in which case it is
-- T (top).
data Range = Range I
           | T
  deriving (Eq, Ord)

instance Pretty Range where
    ppr (Range i) = text "[0," <+> text "max" <+> ppr i <> char ')'
    ppr T         = text "[0,+ifty)"

-- | Calculate a range from an expression that specifies the limit of a loop
-- bound.
rangeE :: MonadInterp m Range => Exp -> m Range
rangeE (VarE v) = do
  maybe_d <- lookupVar v
  case maybe_d of
    Just d  -> return d
    Nothing -> return $ Range $ I Nothing (Set.singleton (VarE v))

rangeE (ConstE (Int32C i)) =
    return $ Range $ I (Just (fromIntegral i)) Set.empty

rangeE (BinopE MaxO e1 e2) = do
    d1 <- rangeE e1
    d2 <- rangeE e2
    return $ joinRanges d1 d2

rangeE _ = return $ T

joinRanges :: Range -> Range -> Range
joinRanges (Range (I c1 vs1)) (Range (I c2 vs2)) =
    Range $ I (up c1 c2) (vs1 `Set.union` vs2)
  where
    up Nothing   Nothing   = Nothing
    up (Just c)  Nothing   = Just c
    up Nothing   (Just c)  = Just c
    up (Just c1) (Just c2) = Just (Prelude.max c1 c2)

joinRanges _ _ = T

class (Applicative m, Monad m, MonadIO m) => MonadInterp m a where
    lookupVar  :: Var -> m (Maybe a)
    extendVars :: [(Var, a)] -> m b -> m b

mergeBounds :: forall m . (MonadInterp m Range) => Traversal AST m
mergeBounds ProgKA (ParforK vs es p) = do
    (ds, es')  <- unzip <$> mapM simplBound (vs `zip` es)
    extendVars (vs `zip` ds) $ do
    ParforK vs es' <$> (fromMnfK <$> go (toMnfK p))
  where
    simplBound :: (Var, Exp) -> m (Range, Exp)
    simplBound (_, e) = do
        e' <- mergeBounds ExpA e
        d  <- rangeE e'
        case d of
          Range i -> return (d, iToExp i)
          T       -> return (d, e')

    go :: Mnf ProgK -> m (Mnf ProgK)
    go [] =
        return []

    go ((seq1, IfThenElseK e1 p1a p1b):(seq2, IfThenElseK e2 p2a p2b):ms) | e1 ==! e2 =
        go ((seq2, IfThenElseK e1 (seq' p1a p2a) (seq' p1b p2b)):ms)
      where
        seq' = case seq1 of
                 ParM -> parK
                 _    -> seqK

    go ((_, IfThenElseK e1 p1a p1b):(_, SyncK):(seq3, IfThenElseK e2 p2a p2b):ms) | e1 ==! e2 =
        go ((seq3, IfThenElseK e1 (sync p1a p2a) (sync p1b p2b)):ms)
      where
        sync m1 m2 = m1 `seqK` SyncK `seqK` m2

    go ((s, m@(IfThenElseK (BinopE LtO (VarE v) e) p1 _)):ms) = do
        d1 <- lookupVar v
        d2 <- Just <$> rangeE e
        if d1 == d2
          then go ((s,p1):ms)
          else do  m'  <- mergeBounds ProgKA m
                   ms' <- go ms
                   return $ (s,m') : ms'

    go ((s,m) : ms) = do
        m'  <- mergeBounds ProgKA m
        ms' <- go ms
        return $ (s,m') : ms'

mergeBounds w a = traverseFam mergeBounds w a

-- The optimization monad
data AEnv = AEnv
    { aVars:: Map Var Range }

defaultAEnv :: AEnv
defaultAEnv = AEnv { aVars = Map.empty }

newtype A a = A { unA :: StateT AEnv IO a }
  deriving (Monad, Functor, Applicative, MonadState AEnv, MonadIO)

evalA :: A a -> IO a
evalA m = evalStateT (unA m) defaultAEnv

instance MonadInterp A Range where
    lookupVar v = gets $ \s -> Map.lookup v (aVars s)

    extendVars vds act = do
        old_vars <- gets aVars
        modify $ \s -> s { aVars = foldl' insert (aVars s) vds }
        x  <- act
        modify $ \s -> s { aVars = old_vars }
        return x
      where
        insert m (k, v) = Map.insert k v m

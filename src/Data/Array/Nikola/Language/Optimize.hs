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

module Data.Array.Nikola.Language.Optimize
    ( optimizeHostProgram
    , liftHostProgram
    , mergeParfor
    , whenE
    , bind
    , binds
    , subst
    ) where

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

import Data.Array.Nikola.Language.Check
import Data.Array.Nikola.Language.Generic
import Data.Array.Nikola.Language.Monad
import Data.Array.Nikola.Language.Optimize.CSE
import Data.Array.Nikola.Language.Optimize.Inliner
import Data.Array.Nikola.Language.Optimize.Monad
import Data.Array.Nikola.Language.Optimize.Occ
import Data.Array.Nikola.Language.Optimize.Simpl
import Data.Array.Nikola.Language.Optimize.Subst
import Data.Array.Nikola.Language.Syntax

-- Host program optimization. Some passes rely on normalized monadic structure,
-- so we perform an initial normalization pass and then do another final
-- normalization pass after other optimizations have had a chance to run.

optimizeHostProgram :: Exp -> R r Exp
optimizeHostProgram =
    return
    >=> pprStage "Start"
    >=> oPass (norm ExpA)
    -- >=> whenDialect CUDA (oPass (mergeParfor ExpA))
    >=> oPass (shareBindings ExpA)
    >=> aPass (mergeBounds ExpA)
    >=> oPass (norm ExpA)

    >=> pprStage "Pre-simplify pass 1"
    >=> oPass (simpl ExpA)
    >=> pprStage "Simplify pass 1"
    >=> cse ExpA
    >=> pprStage "CSE pass 1"
    >=> oPass (occ ExpA)
    >=> pprStage "Occ pass 1"
    >=> oPass (inliner ExpA)
    >=> pprStage "Inliner pass 1"

    >=> oPass (norm ExpA)

    >=> pprStage "Pre-simplify pass 2"
    >=> oPass (simpl ExpA)
    >=> pprStage "Simplify pass 2"
    >=> cse ExpA
    >=> pprStage "CSE pass 2"
    >=> oPass (occ ExpA)
    >=> oPass (inliner ExpA)
    >=> pprStage "Inliner pass 2"

    >=> whenDialect CUDA (constructKernels ExpA)
    >=> oPass (lambdaLift ExpA)

liftHostProgram :: Exp -> R r Exp
liftHostProgram =
    return
    >=> oPass (lambdaLift ExpA)

whenDialect :: Dialect
            -> (Exp -> R r Exp)
            -> Exp
            -> R r Exp
whenDialect dialect f p = do
    flags <- getFlags
    if (fromLJust fDialect flags == dialect)
      then f p
      else return p

oPass :: (Exp -> O Exp) -> Exp -> R r Exp
oPass f = liftIO . evalO . f

aPass :: (Exp -> A Exp) -> Exp -> R r Exp
aPass f = liftIO . evalA . f

pprIO :: MonadIO m => Doc -> m ()
pprIO = liftIO . putStrLn . pretty 200

pprStage :: MonadIO m => String -> Exp -> m Exp
pprStage desc e =
    if False
    then do pprIO $ nest 4 $ text desc </> line </> ppr e
            return e
    else return e

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

-- Kernel construction

constructKernels :: AST a -> a -> R r a
constructKernels = go
  where
    go :: AST a -> a -> R r a
    go ExpA e@(ForE forloop _ _ _) | isParFor forloop = do
        return (CallE (LamE [] e) [])

    go w a = checkTraverseFam go w a

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

mergeParfor ExpA (LamE vtaus p) = do
    LamE vtaus <$> (fromMnf <$> go (toMnf p))
  where
    go :: Mnf Exp -> m (Mnf Exp)
    go [] =
        return []

    go ((seq1, ForE ParFor [v1] [e1] p1) : (seq2, ForE ParFor [v2] [e2] p2) : ms) = do
        insertSubst VarA v2 VarA v1
        let p1' = whenE ((E . VarE) v1 <* (E e1 :: E.Exp t Int32)) p1
        let p2' = whenE ((E . VarE) v1 <* (E e2 :: E.Exp t Int32)) p2
        go ((seq1, ForE ParFor [v1] [BinopE MaxO e1 e2] (sync p1' p2')) : ms)
      where
        sync = case seq2 of
                 SeqM -> syncE
                 ParM -> seqE
                 _    -> error "mergeParfor: saw bind between parallel fors"

    go ((s,m) : ms) = do
        m'  <- mergeParfor ExpA m
        ms' <- go ms
        return $ (s,m') : ms'

mergeParfor w      a    = traverseFam mergeParfor w a

whenE :: E.Exp t a -> Exp -> Exp
whenE e p = IfThenElseE (unE e) p (ReturnE UnitE)

-- Convert monadic actions to a normal form
data SeqM = LetM Var Type
          | SeqM
          | ParM
          | BindM Var Type

instance Pretty SeqM where
    ppr (LetM v tau)  = text "let" <+> ppr v <+> text "::" <+> ppr tau <+>
                        text "="
    ppr SeqM          = text ">>"
    ppr ParM          = text "||"
    ppr (BindM v tau) = text ">>=" <+> text "\\" <+> ppr v <+> text "::" <+> ppr tau <+> text "->"

type Mnf a = [(SeqM, a)]

instance Pretty a => Pretty (Mnf a) where
    ppr [] = empty
    ppr ((LetM v tau, e):ms) = text "let" <+> ppr v <+> text "::" <+> ppr tau <+>
                               text "=" <+> ppr e </> ppr ms
    ppr ((s, m):ms)          = ppr m </> ppr s </> ppr ms

toMnf :: Exp -> Mnf Exp
toMnf (LetE v tau _ e m)            = (LetM v tau, e) : toMnf m

toMnf (SeqE (ReturnE {}) m)         = toMnf m
toMnf (SeqE (SeqE m1 m2) m3)        = toMnf (SeqE m1 (SeqE m2 m3))
toMnf (SeqE (ParE m1 m2) m3)        = toMnf (SeqE m1 (ParE m2 m3))
toMnf (SeqE (BindE v tau m1 m2) m3) = toMnf (BindE v tau m1 (SeqE m2 m3))
toMnf (SeqE m1 m2)                  = (SeqM, m1) : toMnf m2

toMnf (ParE (ReturnE {}) m)         = toMnf m
toMnf (ParE (SeqE m1 m2) m3)        = toMnf (ParE m1 (SeqE m2 m3))
toMnf (ParE (ParE m1 m2) m3)        = toMnf (ParE m1 (ParE m2 m3))
toMnf (ParE (BindE v tau m1 m2) m3) = toMnf (BindE v tau m1 (ParE m2 m3))
toMnf (ParE m1 m2)                  = (ParM, m1) : toMnf m2

toMnf (BindE v tau (SeqE m1 m2) m3) = toMnf (SeqE m1 (BindE v tau m2 m3))
toMnf (BindE v tau (ParE m1 m2) m3) = toMnf (ParE m1 (BindE v tau m2 m3))
toMnf (BindE v2 tau2
          (BindE v1 tau1 m1 m2) m3) = toMnf (BindE v1 tau1 m1 (BindE v2 tau2 m2 m3))
toMnf (BindE v tau m1 m2)           = (BindM v tau, m1) : toMnf m2

toMnf m                             = [(SeqM, m)]

fromMnf :: Mnf Exp -> Exp
fromMnf []              = error "fromMnf: empty list"
fromMnf [(LetM {}, _)]  = error "fromMnf: last action is a let"
fromMnf [(SeqM, m)]     = m
fromMnf [(ParM, _)]     = error "fromMnf: last action is a par"
fromMnf [(BindM {}, _)] = error "fromMnf: last action is a bind"

fromMnf ((LetM v tau, e) :ms) = LetE v tau Many e $ fromMnf ms
fromMnf ((SeqM, m)       :ms) = SeqE m $ fromMnf ms
fromMnf ((ParM, m)       :ms) = ParE m $ fromMnf ms
fromMnf ((BindM v tau, m):ms) = BindE v tau m $ fromMnf ms

-- Normalize monadic actions

norm :: (MonadCheck m, MonadSubst Var Var m, MonadSubst Var Exp m) => AST a -> a -> m a
norm VarA v        = lookupSubst VarA v VarA (return v)
norm ExpA (VarE v) = lookupSubst VarA v ExpA (VarE <$> norm VarA v)

norm ExpA (SeqE (ReturnE {}) m)                    = norm ExpA m
norm ExpA (SeqE (SeqE m1 m2) m3)                   = norm ExpA (SeqE m1 (SeqE m2 m3))
norm ExpA (ParE (ParE m1 m2) m3)                   = norm ExpA (ParE m1 (ParE m2 m3))
norm ExpA (SeqE (BindE v tau m1 m2) m3)            = norm ExpA (BindE v tau m1 (SeqE m2 m3))
norm ExpA (SeqE m1 (ReturnE UnitE))                = do  m1' <- norm ExpA m1
                                                         tau <- inferExp m1' >>= checkMT
                                                         if tau == unitT
                                                           then norm ExpA m1
                                                           else norm ExpA $ SeqE m1' (ReturnE UnitE)
norm ExpA (BindE v _ (ReturnE e) m)                = do  insertSubst VarA v ExpA e
                                                         norm ExpA m
norm ExpA (BindE v tau (SeqE m1 m2) m3)            = norm ExpA (SeqE m1 (BindE v tau m2 m3))
norm ExpA (BindE v2 tau2 (BindE v1 tau1 m1 m2) m3) = norm ExpA (BindE v1 tau1 m1 (BindE v2 tau2 m2 m3))
norm ExpA (CallE (LamE [] (ReturnE e)) [])         = do  m <- ReturnE <$> norm ExpA e
                                                         norm ExpA m

norm w a = checkTraverseFam norm w a

-- Lambda-lift kernels

lambdaLift :: forall m a . MonadCheck m => AST a -> a -> m a
lambdaLift ExpA (CallE (LamE vtaus m) es) = do
    let vs' =  Set.toList (fvs ExpA m)
    taus'   <- mapM lookupVarType vs'
    return $ CallE (LamE (vtaus ++ vs' `zip` taus') m)
                   (es ++ map VarE vs')

lambdaLift w a =
    checkTraverseFam lambdaLift w a

-- Free variables
fvs :: AST a -> a -> Set Var
fvs w a = fst (vars w a Set.empty)

vars :: Fold AST (Set Var -> (Set Var, Set Var))
vars = go
  where
    go :: Fold AST (Set Var -> (Set Var, Set Var))
    go VarA v                   = useVar v
    go ExpA (LetE v _ _ e1 e2)  = go ExpA e1 `mappend`
                                  bindVar v (go ExpA e2)
    go ExpA (LamE vtaus e)      = bindVars (map fst vtaus) (go ExpA e)
    go ExpA (BindE v _ p1 p2)   = go ExpA p1 `mappend`
                                  bindVar v (go ExpA p2)
    go ExpA (ForE _ vs es p)    = foldMap (go ExpA) es `mappend`
                                  bindVars vs (go ExpA p)
    go w    a                   = foldFam go w a

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
         else do  modifyTheta wa wb $ \s -> s { phi   = Set.insert v (phi s)
                                              , theta = Map.delete v old_theta
                                              }
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

    go VarA w ExpA (LamE vtaus e)         = do  let (vs, taus) = unzip vtaus
                                                binds VarA w vs $ \vs' -> do
                                                LamE (vs' `zip` taus) <$> go VarA w ExpA e

    go VarA w ExpA (BindE v tau p1 p2)    = do  p1' <- go VarA w ExpA p1
                                                bind VarA w v $ \v' -> do
                                                BindE v' tau p1' <$> go VarA w ExpA p2

    go VarA w ExpA (ForE floop vs es p)   = do  es' <- traverse (go VarA w ExpA) es
                                                binds VarA w vs $ \vs' -> do
                                                ForE floop vs' es' <$> go VarA w ExpA p

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
mergeBounds ExpA (ForE ParFor vs es p) = do
    (ds, es')  <- unzip <$> mapM simplBound (vs `zip` es)
    extendVars (vs `zip` ds) $ do
    ForE ParFor vs es' <$> (fromMnf <$> go (toMnf p))
  where
    simplBound :: (Var, Exp) -> m (Range, Exp)
    simplBound (_, e) = do
        e' <- mergeBounds ExpA e
        d  <- rangeE e'
        case d of
          Range i -> return (d, iToExp i)
          T       -> return (d, e')

    go :: Mnf Exp -> m (Mnf Exp)
    go [] =
        return []

    go ((seq1, IfThenElseE e1 p1a p1b):(seq2, IfThenElseE e2 p2a p2b):ms)
      | e1 ==! e2 && not (isLetM seq1) && not (isLetM seq2) =
        go ((seq2, IfThenElseE e1 (p1a `seq'` p2a) (p1b `seq'` p2b)):ms)
      where
        seq' = case seq1 of
                 ParM -> parE
                 _    -> seqE

    go ((seq1, IfThenElseE e1 p1a p1b):(_, SyncE):(seq3, IfThenElseE e2 p2a p2b):ms)
      | e1 ==! e2 && not (isLetM seq1) && not (isLetM seq3) =
        go ((seq3, IfThenElseE e1 (p1a `syncE` p2a) (p1b `syncE` p2b)):ms)

    go ((seq1, m@(IfThenElseE (BinopE LtO (VarE v) e) p1 _)):ms)
      | not (isLetM seq1) = do
        d1 <- lookupVar v
        d2 <- Just <$> rangeE e
        if d1 == d2
          then go ((seq1,p1):ms)
          else do  m'  <- mergeBounds ExpA m
                   ms' <- go ms
                   return $ (seq1,m') : ms'

    go ((seq1,m) : ms) = do
        m'  <- mergeBounds ExpA m
        ms' <- go ms
        return $ (seq1,m') : ms'

    isLetM :: SeqM -> Bool
    isLetM (LetM {}) = True
    isLetM _         = False

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

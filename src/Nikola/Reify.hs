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

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Nikola.Reify (
    REnv(..),
    R(..),
    runR,

    Reifiable(..),
    ReifiableFun(..),
    VApply(..)
  ) where

import Control.Applicative
import Control.Monad.State
import Data.Dynamic
import qualified Data.IntMap as IntMap
import Data.List (foldl',
                  foldl1')
import qualified Data.Map as Map
import qualified Data.Set as Set
import System.Mem.StableName
import Text.PrettyPrint.Mainland

import Nikola.Check
import Nikola.Embeddable
import Nikola.Syntax

type StableNameHash = Int

type Binding = (Var, Rho, DExp)

data REnv = REnv
    {  uniq     :: Int
    ,  vars     :: Map.Map Var Rho
    ,  names    :: IntMap.IntMap [(Dynamic, DExp)]
    ,  bindings :: [Binding]
    }
  deriving (Show)

newtype R a = R { unR :: StateT REnv IO a }
  deriving (Monad, MonadIO, MonadState REnv, Functor)

instance Applicative R where
    pure  = return
    (<*>) = ap

instance MonadUniqueVar R where
    newUniqueVar v = do
        u <- gets uniq
        modify $ \s -> s { uniq = u + 1 }
        return $ Var (v ++ show u)

instance MonadStableExp R where
    lookupStableName sn = gets $ \s ->
        case IntMap.lookup hash (names s) of
          Just m' -> Prelude.lookup (Just sn)
                     [(fromDynamic d,e) | (d,e) <- m']
          Nothing -> Nothing
      where
        hash :: StableNameHash
        hash = hashStableName sn

    insertStableName sn e = modify $ \s ->
        s { names = IntMap.alter add (hashStableName sn) (names s) }
      where
        add :: Maybe [(Dynamic, DExp)] -> Maybe [(Dynamic, DExp)]
        add Nothing   = Just [(toDyn sn, e)]
        add (Just xs) = Just ((toDyn sn, e) : xs)

runR  ::  R a
      ->  IO a
runR m = evalStateT (unR m) emptyREnv
  where
    emptyREnv :: REnv
    emptyREnv = REnv
        {  uniq      = 0
        ,  vars      = Map.empty
        ,  names     = IntMap.empty
        ,  bindings  = []
        }

instance MonadCheck R where
    lookupVar v = do
        maybe_tau <- gets $ \s -> Map.lookup v (vars s)
        case maybe_tau of
          Just tau -> return tau
          Nothing ->  faildoc $ text "Variable" <+> ppr v <+>
                                text "not in scope"

    extendVars vtaus act = do
        old_vars <- gets vars
        modify $ \s -> s { vars = foldl' insert (vars s) vtaus }
        x  <- act
        modify $ \s -> s { vars = old_vars }
        return x
      where
        insert m (k, v) = Map.insert k v m

gensym :: R Var
gensym = newUniqueVar "x"

insertBinding :: Var -> DExp -> R ()
insertBinding v e = do
    bs  <- gets bindings
    tau <- extendVars [(v,tau) | (v,tau,_) <- bs] $ check e
    modify $ \s -> s { bindings = (v,tau,e) : bs }

collectBindings :: R a -> R ([Binding], a)
collectBindings act = do
    bs <- gets bindings
    modify $ \s -> s { bindings = []}
    x   <- extendVars [(v,tau) | (v,tau,_) <- bs] $ act
    bs' <- gets bindings
    modify $ \s -> s { bindings = bs }
    return (bs', x)

flushBindings :: R DExp -> R DExp
flushBindings act = do
    (bs, e) <- collectBindings act
    return $ letBind bs e

maximizeSharing :: (DExp -> R DExp)
                -> [DExp]
                -> R [DExp]
maximizeSharing f es = do
    (bs, es') <- collectBindings $ mapM f es
    -- @bs@ is the list of all new bindings created during the the construction
    -- of es'. Note that the order of @bs@ is important; a binding in @bs@ can
    -- only depend on a binding that occurs later in the list.
    let ves      = Map.fromList [(v, e) | (v, _, e) <- bs]
    -- Calculate the set of variables in bs that are needed by each expression.
    let used     = map (closure ves . Set.toList . freeVars) es'
    -- Calculate the set of variables in bs that are needed by all expressions.
    let vshared  = foldl1' Set.intersection used
    -- Split the bindings into two sets: the set that can be shared among all
    -- expressions without duplicating works, and the rest. Throw the shared
    -- bindings back into the pool.
    let (shared, unshared) = split (\(v, _, _) -> Set.member v vshared) bs
    modify $ \s -> s { bindings = shared ++ bindings s }
    -- For each expression, determine which of the remaining bindings are needed
    -- for the expression, and bind them.
    return $ map (bind unshared) (used `zip` es')
  where
    closure :: Map.Map Var DExp -> [Var] -> Set.Set Var
    closure ves vs = loop vs Set.empty
      where
        loop :: [Var] -> Set.Set Var -> Set.Set Var
        loop [] theta = theta
        loop (v : vs) theta | v `Set.member` theta = loop vs theta
                            | otherwise            = loop (fvs ++ vs) (Set.insert v theta)
          where
            fvs = case Map.lookup v ves of
                    Nothing -> []
                    Just e -> Set.toList (freeVars e)

    bind :: [Binding] -> (Set.Set Var, DExp) -> DExp
    bind unshared (used, e) =
        letBind bs e
      where
        bs = filter (\(v, _, _) -> Set.member v used) unshared

    -- Like partition, but we need to maintain the order of the sublists.
    split :: (a -> Bool) -> [a] -> ([a], [a])
    split _ []                 = ([], [])
    split p (x:xs) | p x       = (x:ts', fs')
                   | otherwise = (ts', x:fs')
      where
        (ts', fs') = split p xs

letBind :: [Binding] -> DExp -> DExp
letBind  []                body = body
letBind  ((v,tau,e) : bs)  body = letBind bs $ LetE v tau e body

reifyR :: DExp -> R DExp
reifyR e = do
    sn <-       liftIO $ makeStableName $! e
    maybe_e' <- lookupStableName sn
    case maybe_e' of
      Just e' -> return e'
      Nothing -> do  e' <- mapReifyR e >>= bind
                     insertStableName sn e'
                     return e'
  where
    bind :: DExp -> R DExp
    bind (VarE v)   = return $ VarE v
    bind (BoolE n)  = return $ BoolE n
    bind (IntE n)   = return $ IntE n
    bind (FloatE n) = return $ FloatE n
    bind e          = do  v <- newUniqueVar "v"
                          insertBinding v e
                          return (VarE v)

mapReifyR :: DExp
          -> R DExp
mapReifyR (VarE v) =
    pure $ VarE v

mapReifyR (DelayedE comp) =
    comp

mapReifyR (LetE v _ e1 e2) = do
    e1' <- reifyR e1
    insertBinding v e1'
    reifyR e2

mapReifyR (LamE vtaus e) = do
    e' <- extendVars vtaus $
          flushBindings $
          reifyR e
    return $ LamE vtaus e'

mapReifyR (AppE e es) =
    AppE <$> reifyR e <*> mapM reifyR es

mapReifyR (BoolE b) =
    pure $ BoolE b

mapReifyR (IntE n) =
    pure $ IntE n

mapReifyR (FloatE n) =
    pure $ FloatE n

mapReifyR (UnopE op e ) =
    UnopE op <$> reifyR e

mapReifyR (BinopE op e1 e2) =
    BinopE op <$> reifyR e1 <*> reifyR e2

mapReifyR (IfteE e1 e2 e3) = do
  e1'        <- reifyR e1
  [e2', e3'] <- maximizeSharing reifyR [e2, e3]
  return $ IfteE e1' e2' e3'

mapReifyR (MapE f e) =
    MapE <$> mapReifyR f <*> reifyR e

mapReifyR (MapME f xs ys) =
    MapME <$> mapReifyR f <*> reifyR xs <*> reifyR ys

mapReifyR (PermuteE xs is) =
    PermuteE <$> mapReifyR xs <*> reifyR is

mapReifyR (PermuteME xs is ys) =
    PermuteME <$> mapReifyR xs <*> reifyR is <*> reifyR ys

mapReifyR (ZipWithE f e1 e2) =
    ZipWithE <$> mapReifyR f <*> reifyR e1 <*> reifyR e2

mapReifyR (ZipWith3E f e1 e2 e3) =
    ZipWith3E <$> mapReifyR f <*> reifyR e1 <*> reifyR e2 <*> reifyR e3

mapReifyR (ZipWith3ME f e1 e2 e3 e4) =
    ZipWith3ME <$> mapReifyR f <*>
        reifyR e1 <*> reifyR e2 <*> reifyR e3 <*> reifyR e4

mapReifyR (ScanE f z e) =
    ScanE <$> mapReifyR f <*> reifyR z <*> reifyR e

mapReifyR (BlockedScanME f z e) =
    BlockedScanME <$> mapReifyR f <*> reifyR z <*> reifyR e

mapReifyR (BlockedNacsME f z e) =
    BlockedNacsME <$> mapReifyR f <*> reifyR z <*> reifyR e

mapReifyR (BlockedAddME xs sums) =
    BlockedAddME <$> reifyR xs <*> reifyR sums

class (Typeable a, Typeable b)
  => ReifiableFun a b where
    reifyfun :: (a -> b) -> R DExp
    reifyfun = reifyfunk []

    reifyfunk :: [(Var, Rho)] -> (a -> b) -> R DExp

instance (Embeddable a, Embeddable b)
  => ReifiableFun (Exp a) (Exp b) where
    reifyfunk xrhos f = do
        x          <- gensym
        let rho    =  embeddedType (undefined :: a) (length xrhos)
        let xrhos' =  reverse ((x, rho) : xrhos)
        body       <- extendVars [(x, rho)] $
                      flushBindings $
                      reifyR (unE (f (E (VarE x))))
        -- This is a hack. Eliminating a pointless let binding allows our simple
        -- syntaxtic check for a function body consisting of a single looping
        -- construct to work, thereby enabling running the kernel on a
        -- non-degenerate grid.
        case body of
          LetE v _ e (VarE v') | v' == v ->  return $ LamE xrhos' e
          _ ->                               return $ LamE xrhos' body

instance (Embeddable a, Embeddable b)
  => ReifiableFun (Exp a) (IO (Exp b)) where
    reifyfunk xrhos f = do
        x          <- gensym
        fofx       <- liftIO $ f (E (VarE x))
        let rho    =  embeddedType (undefined :: a) (length xrhos)
        let xrhos' =  reverse ((x, rho) : xrhos)
        body       <- extendVars [(x, rho)] $
                      flushBindings $
                      reifyR (unE fofx)
        -- This is a hack. Eliminating a pointless let binding allows our simple
        -- syntaxtic check for a function body consisting of a single looping
        -- construct to work, thereby enabling running the kernel on a
        -- non-degenerate grid.
        case body of
          LetE v _ e (VarE v') | v' == v ->  return $ LamE xrhos' e
          _ ->                               return $ LamE xrhos' body

instance (Embeddable a, ReifiableFun b c)
  => ReifiableFun (Exp a) (b -> c) where
    reifyfunk xrhos f = do
        x       <- gensym
        let rho =  embeddedType (undefined :: a) (length xrhos)
        extendVars [(x, rho)] $
          reifyfunk ((x, rho) : xrhos) (f (E (VarE x)))

class Reifiable a where
    reify :: a -> IO DExp

instance Reifiable DExp where
    reify = runR . flushBindings . reifyR

instance Reifiable (Exp a) where
    reify = reify . unE

instance ReifiableFun a b => Reifiable (a -> b) where
    reify = runR . reifyfun

class (ReifiableFun a b) => VApply a b c d |  a -> c,
                                              b -> d,
                                              c -> a,
                                              d -> b where
    vapply :: (a -> b) -> c -> d
    vapply f = vapplyk (DelayedE (comp >>= reifyR)) []
      where
        comp :: R DExp
        comp = do
            sn <-       liftIO $ makeStableName $! f
            maybe_e <-  lookupStableName sn
            case maybe_e of
              Just e ->   return e
              Nothing ->  do  e <- reifyfun f
                              insertStableName sn e
                              return e

    vapplyk :: DExp -> [DExp] -> c -> d

instance (Embeddable a, Embeddable b)
  => VApply (Exp a) (Exp b) (Exp a) (Exp b) where
    vapplyk f es = \e -> E $ AppE f (reverse (unE e : es))

instance (Embeddable a, VApply b c d e)
  => VApply (Exp a) (b -> c) (Exp a) (d -> e) where
    vapplyk f es = \e -> vapplyk f (unE e : es)

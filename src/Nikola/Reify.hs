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
    ROpts(..),
    defaultROpts,

    Reifiable(..),
    ReifiableFun(..),
    VApply(..)
  ) where

import Control.Applicative
import Control.Monad.State
import Data.Dynamic
import Data.List (foldl1')
import qualified Data.Map as Map
import qualified Data.Set as Set
import System.Mem.StableName

import Nikola.Check
import Nikola.Representable
import Nikola.Reify.Monad
import Nikola.Syntax

type Binding = (Var, Tau, DExp)

insertBinding :: Var -> DExp -> R ()
insertBinding v e = do
    bs  <- gets rBindings
    tau <- extendVars [(v,tau) | (v,tau,_) <- bs] $ check e
    modify $ \s -> s { rBindings = (v,tau,e) : bs }

collectBindings :: R a -> R ([Binding], a)
collectBindings act = do
    bs <- gets rBindings
    modify $ \s -> s { rBindings = []}
    x   <- extendVars [(v,tau) | (v,tau,_) <- bs] $ act
    bs' <- gets rBindings
    modify $ \s -> s { rBindings = bs }
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
    modify $ \s -> s { rBindings = shared ++ rBindings s }
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

-- Given a "thing" @x@ and a computation @comp@ that computes its translation to
-- a @DExp@, see if we already have a cached translation for @x@. If we do,
-- return it, otherwise compute it via @comp@ and then cache and return the
-- result.
cacheDExp :: Typeable a => a -> R DExp -> R DExp
cacheDExp x comp = do
    sn       <- liftIO $ makeStableName $! x
    maybe_e' <- lookupStableName sn
    case maybe_e' of
      Just e' -> return e'
      Nothing -> do  e' <- comp
                     insertStableName sn e'
                     return e'

-- The main reification function. Note that we DO NOT cache the translations of
-- the function argument to expressions like MapE and ZipWithE because we want
-- the function in its original lambda form---the bodies of the lambda get
-- turned into the body of a loop rather than the body of a function.
reifyR :: DExp -> R DExp
reifyR e = do
    obSharing <- getObserveSharing
    if obSharing then observeSharingReifyR else ignoreSharingReifyR
  where
    observeSharingReifyR :: R DExp
    observeSharingReifyR = cacheDExp e (go e >>= bind)
      where
        bind :: DExp -> R DExp
        bind (VarE v)   = return $ VarE v
        bind (BoolE n)  = return $ BoolE n
        bind (IntE n)   = return $ IntE n
        bind (FloatE n) = return $ FloatE n
        bind e          = do  v <- newUniqueVar "v"
                              insertBinding v e
                              return (VarE v)

    ignoreSharingReifyR :: R DExp
    ignoreSharingReifyR = go e

    go :: DExp -> R DExp
    go (VarE v) =
        pure $ VarE v

    go (DelayedE comp) =
        comp

    go (LetE v _ e1 e2) = do
        e1' <- reifyR e1
        insertBinding v e1'
        reifyR e2

    go (LamE vtaus e) = do
        e' <- extendVars vtaus $
              flushBindings $
              reifyR e
        return $ LamE vtaus e'

    go (AppE e es) =
        AppE <$> reifyR e <*> mapM reifyR es

    go (BoolE b) =
        pure $ BoolE b

    go (IntE n) =
        pure $ IntE n

    go (FloatE n) =
        pure $ FloatE n

    go (UnopE op e ) =
        UnopE op <$> reifyR e

    go (BinopE op e1 e2) =
        BinopE op <$> reifyR e1 <*> reifyR e2

    go (IfteE e1 e2 e3) = do
      e1'        <- reifyR e1
      [e2', e3'] <- maximizeSharing reifyR [e2, e3]
      return $ IfteE e1' e2' e3'

    go (MapE f e) =
        MapE <$> go f <*> reifyR e

    go (MapME f xs ys) =
        MapME <$> go f <*> reifyR xs <*> reifyR ys

    go (PermuteE xs is) =
        PermuteE <$> go xs <*> reifyR is

    go (PermuteME xs is ys) =
        PermuteME <$> go xs <*> reifyR is <*> reifyR ys

    go (ZipWithE f e1 e2) =
        ZipWithE <$> go f <*> reifyR e1 <*> reifyR e2

    go (ZipWith3E f e1 e2 e3) =
        ZipWith3E <$> go f <*> reifyR e1 <*> reifyR e2 <*> reifyR e3

    go (ZipWith3ME f e1 e2 e3 e4) =
        ZipWith3ME <$> go f <*>
            reifyR e1 <*> reifyR e2 <*> reifyR e3 <*> reifyR e4

    go (ScanE f z e) =
        ScanE <$> go f <*> reifyR z <*> reifyR e

    go (BlockedScanME f z e) =
        BlockedScanME <$> go f <*> reifyR z <*> reifyR e

    go (BlockedNacsME f z e) =
        BlockedNacsME <$> go f <*> reifyR z <*> reifyR e

    go (BlockedAddME xs sums) =
        BlockedAddME <$> reifyR xs <*> reifyR sums

class (Typeable a, Typeable b)
  => ReifiableFun a b where
    reifyfun :: (a -> b) -> R DExp
    reifyfun = reifyfunk []

    reifyfunk :: [(Var, Tau)] -> (a -> b) -> R DExp

instance (Representable a, Representable b)
  => ReifiableFun (Exp a) (Exp b) where
    reifyfunk xrhos f = do
        x          <- gensym
        let rho    =  embeddedType (undefined :: a) (ParamIdx (length xrhos))
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

instance (Representable a, Representable b)
  => ReifiableFun (Exp a) (IO (Exp b)) where
    reifyfunk xrhos f = do
        x          <- gensym
        fofx       <- liftIO $ f (E (VarE x))
        let rho    =  embeddedType (undefined :: a) (ParamIdx (length xrhos))
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

instance (Representable a, ReifiableFun b c)
  => ReifiableFun (Exp a) (b -> c) where
    reifyfunk xrhos f = do
        x       <- gensym
        let rho =  embeddedType (undefined :: a) (ParamIdx (length xrhos))
        extendVars [(x, rho)] $
          reifyfunk ((x, rho) : xrhos) (f (E (VarE x)))

class Reifiable a where
    reify :: a -> IO DExp
    reify = reifyEx defaultROpts

    reifyEx :: ROpts -> a -> IO DExp

instance Reifiable DExp where
    reifyEx ropts = runR ropts . flushBindings . reifyR

instance Reifiable (Exp a) where
    reifyEx ropts = reifyEx ropts . unE

instance ReifiableFun a b => Reifiable (a -> b) where
    reifyEx ropts = runR ropts . reifyfun

class (ReifiableFun a b) => VApply a b c d |  a -> c,
                                              b -> d,
                                              c -> a,
                                              d -> b where
    vapply :: (a -> b) -> c -> d
    vapply f = vapplyk (DelayedE (cacheDExp f (reifyfun f))) []

    vapplyk :: DExp -> [DExp] -> c -> d

instance (Representable a, Representable b)
  => VApply (Exp a) (Exp b) (Exp a) (Exp b) where
    vapplyk f es = \e -> E $ AppE f (reverse (unE e : es))

instance (Representable a, VApply b c d e)
  => VApply (Exp a) (b -> c) (Exp a) (d -> e) where
    vapplyk f es = \e -> vapplyk f (unE e : es)

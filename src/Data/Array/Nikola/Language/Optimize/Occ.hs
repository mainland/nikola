{-# LANGUAGE GADTs #-}

-- |
-- Module      : Data.Array.Nikola.Language.Optimize.Occ
-- Copyright   : (c) Geoffrey Mainland 2012
-- License     : BSD-style
--
-- Maintainer  : Geoffrey Mainland <mainland@apeiron.net>
-- Stability   : experimental
-- Portability : non-portable

module Data.Array.Nikola.Language.Optimize.Occ (occ) where

import Control.Monad.State (gets, modify)
import Data.Foldable
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Traversable

import Data.Array.Nikola.Language.Generic
import Data.Array.Nikola.Language.Optimize.Monad
import Data.Array.Nikola.Language.Syntax

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

unionOcc :: Map Var Occ -> O ()
unionOcc occ =
    modify $ \s -> s { oOcc = oOcc s `occsJoin` occ }

occ :: AST a -> a -> O a
occ ExpA e@(VarE v) = do
    occVar v
    return e

occ ExpA (LetE v tau _ e1 e2) = do
    (e1', occ1) <- withOcc $ occ ExpA e1
    (e2', occ2) <- withOcc $ occ ExpA e2
    let occ = Map.findWithDefault Never v occ2
    unionOcc $ occ1 `occsJoin` occsDelete [v] occ2
    return $ LetE v tau occ e1' e2'

occ ExpA (LamE vtaus e) = do
    (e', occ) <- withOcc $ occ ExpA e
    unionOcc $ occsDelete (map fst vtaus) occ
    return $ LamE vtaus e'

occ ExpA (IfThenElseE e1 e2 e3) = do
    (e1', occ1) <- withOcc $ occ ExpA e1
    (e2', occ2) <- withOcc $ occ ExpA e2
    (e3', occ3) <- withOcc $ occ ExpA e3
    unionOcc $ occ1 `occsJoin` (occ2 `occsMeet` occ3)
    return $ IfThenElseE e1' e2' e3'

occ ExpA (ForE isPar vs es p) = do
    (es', occ_es) <- withOcc $ traverse (occ ExpA) es
    (p',  occ_p)  <- withOcc $ occ ExpA p
    unionOcc $ occ_es `occsJoin` occsDelete vs occ_p
    return $ ForE isPar vs es' p'

occ ExpA (BindE v tau p1 p2) = do
    (p1', occ1) <- withOcc $ occ ExpA p1
    (p2', occ2) <- withOcc $ occ ExpA p2
    unionOcc (occ1 `occsJoin` occ2)
    return $ BindE v tau p1' p2'

occ w a = traverseFam occ w a

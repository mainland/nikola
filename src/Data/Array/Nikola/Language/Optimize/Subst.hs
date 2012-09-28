{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module      : Data.Array.Nikola.Language.Optimize.Subst
-- Copyright   : (c) Geoffrey Mainland 2012
-- License     : BSD-style
--
-- Maintainer  : Geoffrey Mainland <mainland@apeiron.net>
-- Stability   : experimental
-- Portability : non-portable

module Data.Array.Nikola.Language.Optimize.Subst
    ( Binder(..)
    , BinderOcc(..)
    , Theta(..)
    , MonadSubst(..)
    , lookupSubst
    , insertSubst
    ) where

import Control.Applicative (Applicative)
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)

import Data.Array.Nikola.Language.Generic
import Data.Array.Nikola.Language.Syntax

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

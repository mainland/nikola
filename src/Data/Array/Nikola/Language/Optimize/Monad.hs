{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

 -- |
-- Module      : Data.Array.Nikola.Language.Optimize.Monad
-- Copyright   : (c) Geoffrey Mainland 2012
-- License     : BSD-style
--
-- Maintainer  : Geoffrey Mainland <mainland@apeiron.net>
-- Stability   : experimental
-- Portability : non-portable

module Data.Array.Nikola.Language.Optimize.Monad
    ( O
    , OEnv(..)
    , evalO
    , runO
    ) where

import Control.Applicative (Applicative)
import Control.Monad.State (StateT(..), evalStateT,
                            MonadState(..), gets, modify)
import Control.Monad.Trans (MonadIO(..))
import Data.Foldable
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Text.PrettyPrint.Mainland

import Data.Array.Nikola.Language.Check
import Data.Array.Nikola.Language.Optimize.Subst
import Data.Array.Nikola.Language.Syntax

data OEnv = OEnv
    { oVarVarSubst :: Map Var Var
    , oVarExpSubst :: Map Var Exp
    , oContext     :: Context
    , oVarTypes    :: Map Var Type
    , oOcc         :: Map Var Occ
    }

defaultOEnv :: OEnv
defaultOEnv = OEnv { oVarVarSubst = Map.empty
                   , oVarExpSubst = Map.empty
                   , oContext     = Host
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
    getContext = gets oContext

    setContext ctx = modify $ \s -> s { oContext = ctx }

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

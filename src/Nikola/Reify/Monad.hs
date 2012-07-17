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
--
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

module Nikola.Reify.Monad (
    ROpts(..),
    defaultROpts,
    REnv(..),
    R,
    runR,

    newUniqueVar,
    gensym,
    getObserveSharing,
    lookupStableName,
    insertStableName,
    lookupVar,
    extendVars
  ) where

import Control.Applicative
import Control.Monad.State
import Data.Dynamic
import qualified Data.IntMap as IntMap
import Data.List (foldl')
import qualified Data.Map as Map
import System.Mem.StableName
import Text.PrettyPrint.Mainland

import Nikola.Check
import Nikola.Syntax

type StableNameHash = Int

type Binding = (Var, Rho, DExp)

data ROpts = ROpts { roptObserveSharing :: Bool }
  deriving (Show)

defaultROpts = ROpts { roptObserveSharing = True }

data REnv = REnv
    {  uniq      :: Int
    ,  ropts     :: ROpts
    ,  vars      :: Map.Map Var Rho
    ,  names     :: IntMap.IntMap [(Dynamic, DExp)]
    ,  bindings  :: [Binding]
    }
  deriving (Show)

newtype R a = R { unR :: StateT REnv IO a }
  deriving (Applicative, Functor, Monad, MonadIO, MonadState REnv)

runR  ::  ROpts
      ->  R a
      ->  IO a
runR opts m = evalStateT (unR m) emptyREnv
  where
    emptyREnv :: REnv
    emptyREnv = REnv
        {  uniq      = 0
        ,  ropts     = opts
        ,  vars      = Map.empty
        ,  names     = IntMap.empty
        ,  bindings  = []
        }

newUniqueVar :: String -> R Var
newUniqueVar v = do
    u <- gets uniq
    modify $ \s -> s { uniq = u + 1 }
    return $ Var (v ++ show u)

gensym :: R Var
gensym = newUniqueVar "x"

getObserveSharing :: R Bool
getObserveSharing =
    gets (roptObserveSharing . ropts)

lookupStableName :: Typeable a => StableName a -> R (Maybe DExp)
lookupStableName sn = gets $ \s ->
    case IntMap.lookup hash (names s) of
      Just m' -> Prelude.lookup (Just sn)
                 [(fromDynamic d,e) | (d,e) <- m']
      Nothing -> Nothing
  where
    hash :: StableNameHash
    hash = hashStableName sn

insertStableName :: Typeable a => StableName a -> DExp -> R ()
insertStableName sn e = modify $ \s ->
    s { names = IntMap.alter add (hashStableName sn) (names s) }
  where
    add :: Maybe [(Dynamic, DExp)] -> Maybe [(Dynamic, DExp)]
    add Nothing   = Just [(toDyn sn, e)]
    add (Just xs) = Just ((toDyn sn, e) : xs)

instance  MonadCheck R where
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

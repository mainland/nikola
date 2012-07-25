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

module Data.Array.Nikola.Language.Reify.Monad (
    ROpts(..),
    defaultROpts,
    REnv(..),
    R,
    runR,

    gensym,
    getObserveSharing,
    lookupStableName,
    insertStableName,

    inNewScope,
    inBranch,

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

import Data.Array.Nikola.Language.Check
import Data.Array.Nikola.Language.Syntax

type StableNameHash = Int

data ROpts = ROpts { roptObserveSharing :: Bool }
  deriving (Show)

defaultROpts = ROpts { roptObserveSharing = True }

data REnv = REnv
    {  rUniq         :: Int
    ,  rOpts         :: ROpts
    ,  rVars         :: Map.Map Var Tau
    ,  rStableNames  :: IntMap.IntMap [(Dynamic, DExp)]
    }
  deriving (Show)

instance Show (StableName DExp) where
    show _ = "StableName DExp"

newtype R a = R { unR :: StateT REnv IO a }
  deriving (Applicative, Functor, Monad, MonadIO, MonadState REnv)

runR  ::  ROpts
      ->  R a
      ->  IO a
runR opts m = evalStateT (unR m) emptyREnv
  where
    emptyREnv :: REnv
    emptyREnv = REnv
        {  rUniq         = 0
        ,  rOpts         = opts
        ,  rVars         = Map.empty
        ,  rStableNames  = IntMap.empty
        }

gensym :: String -> R Var
gensym v = do
    u <- gets rUniq
    modify $ \s -> s { rUniq = u + 1 }
    return $ Var (v ++ show u)

getObserveSharing :: R Bool
getObserveSharing =
    gets (roptObserveSharing . rOpts)

lookupStableName :: Typeable a => StableName a -> R (Maybe DExp)
lookupStableName sn = gets $ \s ->
    case IntMap.lookup hash (rStableNames s) of
      Just m' -> Prelude.lookup (Just sn)
                 [(fromDynamic d,e) | (d,e) <- m']
      Nothing -> Nothing
  where
    hash :: StableNameHash
    hash = hashStableName sn

insertStableName :: Typeable a => StableName a -> DExp -> R ()
insertStableName sn e = modify $ \s ->
    s { rStableNames = IntMap.alter add (hashStableName sn) (rStableNames s) }
  where
    add :: Maybe [(Dynamic, DExp)] -> Maybe [(Dynamic, DExp)]
    add Nothing   = Just [(toDyn sn, e)]
    add (Just xs) = Just ((toDyn sn, e) : xs)

inNewScope :: R a -> R a
inNewScope comp = do
    cache <- gets rStableNames
    modify $ \s -> s { rStableNames = IntMap.empty }
    a     <- comp
    modify $ \s -> s { rStableNames = cache }
    return a

inBranch :: R a -> R a
inBranch comp = do
    cache <- gets rStableNames
    a     <- comp
    modify $ \s -> s { rStableNames = cache }
    return a

instance MonadCheck R where
    lookupVar v = do
        maybe_tau <- gets $ \s -> Map.lookup v (rVars s)
        case maybe_tau of
          Just tau -> return tau
          Nothing ->  faildoc $ text "Variable" <+> ppr v <+>
                                text "not in scope during reification."

    extendVars vtaus act = do
        old_vars <- gets rVars
        modify $ \s -> s { rVars = foldl' insert (rVars s) vtaus }
        x  <- act
        modify $ \s -> s { rVars = old_vars }
        return x
      where
        insert m (k, v) = Map.insert k v m

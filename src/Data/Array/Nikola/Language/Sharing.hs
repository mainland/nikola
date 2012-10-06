{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : Data.Array.Nikola.Language.Sharing
-- Copyright   : (c) The President and Fellows of Harvard College 2009-2010
-- Copyright   : (c) Geoffrey Mainland 2012
-- License     : BSD-style
--
-- Maintainer  : Geoffrey Mainland <mainland@apeiron.net>
-- Stability   : experimental
-- Portability : non-portable

module Data.Array.Nikola.Language.Sharing (
    detectSharing,

    cacheExp
  ) where

import Control.Applicative
import Control.Monad.State
import Data.Dynamic
import System.Mem.StableName
-- import Text.PrettyPrint.Mainland

import Data.Array.Nikola.Backend.Flags
import Data.Array.Nikola.Language.Check
import Data.Array.Nikola.Language.Generic
import Data.Array.Nikola.Language.Monad
import Data.Array.Nikola.Language.Syntax

detectSharing :: AST a -> a -> R r a
detectSharing ExpA e =
    inNewScope False $ detectE ExpA e
  where
    cachedDetectE :: AST a -> a -> R Exp a
    cachedDetectE ExpA e = do
        obSharing <- fromLJust fObsSharing <$> getFlags
        if obSharing
          then cacheExp e (detectE ExpA e)
          else detectE ExpA e

    cachedDetectE w a = checkTraverseFam cachedDetectE w a

    detectE :: AST a -> a -> R Exp a
    detectE ExpA (LetE v tau _ e1 e2) = do
        e1' <- cachedDetectE ExpA e1
        letE v tau e1'
        detectE ExpA e2

    detectE ExpA (LamE vtaus e) =
        inNewScope True $ do
        lamE vtaus $ cachedDetectE ExpA e

    detectE ExpA (IfThenElseE e1 e2 e3) =
        IfThenElseE  <$> cachedDetectE ExpA e1
                     <*> inNewScope False (cachedDetectE ExpA e2)
                     <*> inNewScope False (cachedDetectE ExpA e3)

    detectE ExpA (BindE v tau m1 m2) = do
        m1' <- detectE ExpA m1
        m2' <- extendVarTypes [(v, tau)] $
               inNewScope False $
               detectE ExpA m2
        return $ BindE v tau m1' m2'

    detectE ExpA (ForE isPar vs es body) = do
        es'   <- mapM detectLocal es
        body' <- extendVarTypes (vs `zip` repeat ixT) $
                 inNewScope False $
                 detectE ExpA body
        return $ ForE isPar vs es' body'
      where
        detectLocal :: Exp -> R Exp Exp
        detectLocal e = inNewScope False $ detectE ExpA e

    detectE ExpA (DelayedE comp) =
        comp >>= cachedDetectE ExpA

    detectE w a = checkTraverseFam cachedDetectE w a

detectSharing w a = checkTraverseFam detectSharing w a

-- Given a "thing" @x@ and a computation @comp@ that computes its translation to
-- a @Exp@, see if we already have a cached translation for @x@. If we do,
-- return it, otherwise compute it via @comp@ and then cache and return the
-- result.
cacheExp :: Typeable a
         => a
         -> R Exp Exp
         -> R Exp Exp
cacheExp x comp = do
    sn      <- liftIO $ makeStableName $! x
    maybe_e <- lookupStableName sn
    case maybe_e of
      Just e  -> return e
      Nothing -> do  e <- comp >>= mkLetE
                     insertStableName sn e
                     return e

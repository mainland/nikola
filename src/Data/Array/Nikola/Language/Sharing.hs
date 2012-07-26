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

    detectE ExpA (DelayedE comp) =
        comp >>= cachedDetectE ExpA

    detectE w a = checkTraverseFam cachedDetectE w a

detectSharing ProgKA p =
    reset $ detectK ProgKA p
  where
    cachedDetectK :: AST a -> a -> R ProgK a
    cachedDetectK ExpA   e = detectSharing ExpA e
    cachedDetectK ProgKA p = do
        obSharing <- fromLJust fObsSharing <$> getFlags
        if obSharing
          then cacheProgK p (detectK ProgKA p)
          else detectK ProgKA p

    cachedDetectK w a = checkTraverseFam cachedDetectK w a

    detectK :: AST a -> a -> R ProgK a
    detectK ProgKA (DelayedK comp) =
        comp >>= cachedDetectK ProgKA

    detectK w a = checkTraverseFam cachedDetectK w a

detectSharing ProgHA p =
    reset $ detectH ProgHA p
  where
    cachedDetectH :: AST a -> a -> R ProgH a
    cachedDetectH ExpA p   = detectSharing ExpA p
    cachedDetectH ProgKA p = detectSharing ProgKA p
    cachedDetectH ProgHA p = do
        obSharing <- fromLJust fObsSharing <$> getFlags
        if obSharing
          then cacheProgH p (detectH ProgHA p)
          else detectH ProgHA p

    cachedDetectH w a = checkTraverseFam cachedDetectH w a

    detectH :: AST a -> a -> R ProgH a
    detectH ProgHA (DelayedH comp) =
        comp >>= cachedDetectH ProgHA

    detectH w a = checkTraverseFam cachedDetectH w a

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
    maybe_e <- lookupExp sn
    case maybe_e of
      Just e  -> return e
      Nothing -> do  e <- comp >>= mkLetE
                     insertExp sn e
                     return e

cacheProgH :: Typeable a
           => a
           -> R ProgH ProgH
           -> R ProgH ProgH
cacheProgH x comp = do
    sn      <- liftIO $ makeStableName $! x
    maybe_p <- lookupProgH sn
    case maybe_p of
      Just p  -> return p
      Nothing -> do  p <- comp
                     insertProgH sn p
                     return p

cacheProgK :: Typeable a
           => a
           -> R ProgK ProgK
           -> R ProgK ProgK
cacheProgK x comp = do
    sn      <- liftIO $ makeStableName $! x
    maybe_p <- lookupProgK sn
    case maybe_p of
      Just p  -> return p
      Nothing -> do  p <- comp
                     insertProgK sn p
                     return p

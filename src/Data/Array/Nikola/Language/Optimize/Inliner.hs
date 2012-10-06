{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- Module      : Data.Array.Nikola.Language.Optimize.Inliner
-- Copyright   : (c) Geoffrey Mainland 2012
-- License     : BSD-style
--
-- Maintainer  : Geoffrey Mainland <mainland@apeiron.net>
-- Stability   : experimental
-- Portability : non-portable

module Data.Array.Nikola.Language.Optimize.Inliner (inliner) where

import Control.Applicative

import Data.Array.Nikola.Language.Generic
import Data.Array.Nikola.Language.Optimize.Monad
import Data.Array.Nikola.Language.Optimize.Subst
import Data.Array.Nikola.Language.Syntax

inliner :: AST a -> a -> O a
inliner ExpA (VarE v) =
    lookupSubst VarA v ExpA (return (VarE v))

inliner ExpA (LetE _ _ Never _ e2) =
    inliner ExpA e2

inliner ExpA (LetE v _ Once e1 e2) = do
    e1' <- inliner ExpA e1
    insertSubst VarA v ExpA e1'
    inliner ExpA e2

inliner ExpA (LetE v tau Many e1 e2) = do
    e1' <- inliner ExpA e1
    if isAtomicE e1'
      then do  insertSubst VarA v ExpA e1'
               inliner ExpA e2
      else LetE v tau Many e1' <$> inliner ExpA e2

inliner w a = traverseFam inliner w a

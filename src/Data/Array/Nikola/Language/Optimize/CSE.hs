{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- Module      : Data.Array.Nikola.Language.Optimize.CSE
-- Copyright   : (c) Geoffrey Mainland 2012
-- License     : BSD-style
--
-- Maintainer  : Geoffrey Mainland <mainland@apeiron.net>
-- Stability   : experimental
-- Portability : non-portable

module Data.Array.Nikola.Language.Optimize.CSE (cse) where

import Control.Applicative

import Data.Array.Nikola.Language.Check
import Data.Array.Nikola.Language.Generic
import Data.Array.Nikola.Language.Monad
import Data.Array.Nikola.Language.Syntax

cse :: AST a -> a -> R r a
cse ExpA e =
    inNewScope False $ go ExpA e
  where
    go  :: AST a -> a -> R Exp a
    go ExpA (LetE v tau _ e1 e2) = do
        e1' <- go ExpA e1
        letE v tau e1'
        go ExpA e2

    go ExpA (LamE vtaus e) =
        inNewScope True $ do
        lamE vtaus $ go ExpA e

    go ExpA (IfThenElseE e1 e2 e3) =
        IfThenElseE  <$> go ExpA e1
                     <*> inNewScope False (go ExpA e2)
                     <*> inNewScope False (go ExpA e3)

    go ExpA (ForE isPar vs es body) = do
        es'   <- mapM (go ExpA) es
        body' <- extendVarTypes (vs `zip` repeat ixT) $
                 inNewScope False $
                 go ExpA body
        return $ ForE isPar vs es' body'

    go ExpA e = do
        e'        <- checkTraverseFam go ExpA e
        maybe_e'' <- lookupExp e'
        case maybe_e'' of
          Nothing  -> do  --vtaus <- gets rVarTypes
                          --pprIO $ nest 4 $ text "in scope" </> ppr vtaus
                          --pprIO $ text "Memoizing" <+> ppr e'
                          e'' <- mkLetE e'
                          --pprIO $ nest 4 (text "Memoized" </> ppr e') </> nest 4 (text "as" </> ppr e'')
                          return e''
          Just e'' -> return e''

    go w a = checkTraverseFam go w a

cse w a = checkTraverseFam cse w a

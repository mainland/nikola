{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Data.Array.Nikola.Program
-- Copyright   : (c) Geoffrey Mainland 2012
-- License     : BSD-style
--
-- Maintainer  : Geoffrey Mainland <mainland@apeiron.net>
-- Stability   : experimental
-- Portability : non-portable

module Data.Array.Nikola.Program (
    P,

    seqK
  ) where

-- import Text.PrettyPrint.Mainland

import Data.Array.Nikola.Language.Monad
import Data.Array.Nikola.Language.Syntax hiding (Var, Exp)
import qualified Data.Array.Nikola.Language.Syntax as S

-- | Prepend a monadic action to the program being generated.
seqK :: S.Exp -> a -> P a
seqK p1 x = do
    shift $ \k -> do
    p2 <- reset $ k x
    return $ p1 `seqE` p2

{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module      : Data.Array.Nikola.Eval.Load
-- Copyright   : (c) Geoffrey Mainland 2012
-- License     : BSD-style
--
-- Maintainer  : Geoffrey Mainland <mainland@apeiron.net>
-- Stability   : experimental
-- Portability : non-portable

module Data.Array.Nikola.Eval.Load (
    Load(..),
  ) where

import Data.Array.Nikola.Array
import Data.Array.Nikola.Eval.Target

import Data.Array.Nikola.Language.Monad

class IsArray r1 e => Load r1 sh e where
    loadP :: Target r2 e => Array r1 sh e -> MArray r2 sh e -> P ()

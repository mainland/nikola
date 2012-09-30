{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module      : Data.Array.Nikola.Eval
-- Copyright   : (c) Geoffrey Mainland 2012
-- License     : BSD-style
--
-- Maintainer  : Geoffrey Mainland <mainland@apeiron.net>
-- Stability   : experimental
-- Portability : non-portable

module Data.Array.Nikola.Eval (
    Load(..),
    Target(..),

    computeP,
  ) where

import Data.Array.Nikola.Array
import Data.Array.Nikola.Eval.Load
import Data.Array.Nikola.Eval.Target
import Data.Array.Nikola.Shape

import Data.Array.Nikola.Language.Monad

computeP :: ( Shape sh
            , IsArray r1 e
            , Load   r1 sh e
            , Target r2    e)
         => Array r1 sh e -> P (Array r2 sh e)
computeP arr = do
    arr' <- newMArray (extent arr)
    loadP arr arr'
    unsafeFreezeMArray arr'

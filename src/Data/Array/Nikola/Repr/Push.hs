{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : Data.Array.Nikola.Repr.Push
-- Copyright   : (c) Geoffrey Mainland 2012
-- License     : BSD-style
--
-- Maintainer  : Geoffrey Mainland <mainland@apeiron.net>
-- Stability   : experimental
-- Portability : non-portable

module Data.Array.Nikola.Repr.Push (
    PSH,
    Array(..),

    mkPushArray,
    push
  ) where

import Data.Typeable (Typeable)

import Data.Array.Nikola.Array
import Data.Array.Nikola.Exp
import Data.Array.Nikola.Repr.Manifest
import Data.Array.Nikola.Shape

import Data.Array.Nikola.Language.Monad
import Data.Array.Nikola.Language.Syntax hiding (Var, Exp)

-- | "Push" arrays represented by a program that produces index, value pairs.
data PSH
  deriving (Typeable)

instance IsArray PSH a where
    data Array PSH sh a = APush sh (P (sh, a))

    extent (APush sh _) = sh

instance IsElem a => Manifest PSH a where
    manifest (APush _ m) v = do
    p1 <- reset $ do  (i, x) <- m
                      write v i x
                      return $ ReturnK UnitE
    shift $ \k -> do
    p2 <- reset $ k ()
    return $ p1 `seqK` p2

-- | Construct a push array from a function mapping indices to values.
mkPushArray :: forall sh a . (Shape sh)
            => sh
            -> (sh -> a)
            -> Array PSH sh a
mkPushArray sh f = APush sh m
  where
    m :: P (sh, a)
    m = do  i <- parfor sh
            return (i, f i)

-- | Convert an array into a push array.
push :: (Shape sh, Source r e)
     => Array r sh e
     -> Array PSH sh e
push arr = mkPushArray (extent arr) (index arr)

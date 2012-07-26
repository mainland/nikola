{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : Data.Array.Nikola.Array
-- Copyright   : (c) Geoffrey Mainland 2012
-- License     : BSD-style
--
-- Maintainer  : Geoffrey Mainland <mainland@apeiron.net>
-- Stability   : experimental
-- Portability : non-portable

module Data.Array.Nikola.Array (
    IsArray(..),
    Source(..),
    (!)
  ) where

import Data.Typeable (Typeable, Typeable3)

import Data.Array.Nikola.Exp
import Data.Array.Nikola.Shape

-- | Class of array representations that have a shape.
class Typeable r => IsArray r e where
    -- Arrays with a representation tag, shape, and element type.  Use one of
    -- the type tags like `D`, `U` and so on for @r@, one of `DIM1`, `DIM2`
    -- ... for @sh@.
    data Array r sh e

    -- | O(1). Take the extent (size) of an array.
    extent :: Shape sh => Array r sh e -> sh

deriving instance Typeable3 Array

-- | Class of array representations that we can read elements from.
class IsArray r e => Source r e where
    -- | O(1). Shape polymorphic indexing.
    index, unsafeIndex :: Shape sh => Array r sh e -> sh -> e
    index arr ix = arr `linearIndex` toIndex (extent arr) ix
    unsafeIndex  = index

    -- | O(1). Linear indexing into underlying, row-major, array representation.
    linearIndex, unsafeLinearIndex :: Shape sh
                                   => Array r sh e
                                   -> Exp t Ix
                                   -> e
    unsafeLinearIndex = linearIndex

-- | O(1). Alias for `index`
(!) :: (Shape sh, Source r e) => Array r sh e -> sh -> e
(!) = index

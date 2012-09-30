{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : Data.Array.Nikola.Eval.Target
-- Copyright   : (c) Geoffrey Mainland 2012
-- License     : BSD-style
--
-- Maintainer  : Geoffrey Mainland <mainland@apeiron.net>
-- Stability   : experimental
-- Portability : non-portable

module Data.Array.Nikola.Eval.Target (
    Target(..),
  ) where

import Data.Array.Nikola.Array
import Data.Array.Nikola.Shape

import Data.Array.Nikola.Language.Monad

-- | Class of manifest array representations. Note that unlike Repa, we keep
-- shape information in the 'MArray' associated type. This is because on some
-- targets, e.g., CUDA, shape is actually part of the array representation. For
-- example, CUDA has 2D and 3D arrays that require calling different memory
-- allocators. The situation is similar for CUDA textures.
class Target r e where
    -- | Mutable version of the representation.
    data MArray r sh e

    -- | Allocate a new mutable array of the given size.
    newMArray :: Shape sh => sh -> P (MArray r sh e)

    -- | Write an element into the mutable array.
    unsafeWriteMArray :: Shape sh => MArray r sh e -> sh -> e -> P ()

    -- | Freeze the mutable array into an immutable Nikola array.
    unsafeFreezeMArray :: Shape sh => MArray r sh e -> P (Array r sh e)

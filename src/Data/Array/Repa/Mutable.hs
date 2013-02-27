{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : Data.Array.Repa.Mutable
-- Copyright   : (c) Geoffrey Mainland 2012
-- License     : BSD-style
--
-- Maintainer  : Geoffrey Mainland <mainland@apeiron.net>
-- Stability   : experimental
-- Portability : non-portable
--
-- This module provides an interface for mutable arrays. It is like the 'Target'
-- type class, but maintains shape information.

module Data.Array.Repa.Mutable (Mutable(..)) where

import Data.Array.Repa

class Mutable r sh e where
    -- | Mutable representation of an array
    data MArray r sh e

    -- | Get extent of the mutable array.
    mextent :: MArray r sh e -> sh

    -- | Allocate a new mutable array of the given size.
    newMArray :: sh -> IO (MArray r sh e)

    -- | Write an element into the mutable array.
    unsafeWriteMArray :: MArray r sh e -> sh -> e -> IO ()

    -- | Freeze the mutable array into an immutable Repa array.
    unsafeFreezeMArray :: MArray r sh e -> IO (Array r sh e)

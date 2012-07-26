-- |
-- Module      : Data.Array.Nikola.Util.Bool
-- Copyright   : (c) Geoffrey Mainland 2012
-- License     : BSD-style
--
-- Maintainer  : Geoffrey Mainland <mainland@apeiron.net>
-- Stability   : experimental
-- Portability : non-portable

module Data.Array.Nikola.Util.Bool
    ( fromBool
    , toBool
    ) where

import Data.Word (Word8)

fromBool :: Bool -> Word8
{-# INLINE fromBool #-}
fromBool False = 0
fromBool True  = 1

toBool :: Word8 -> Bool
{-# INLINE toBool #-}
toBool 0 = False
toBool _ = True

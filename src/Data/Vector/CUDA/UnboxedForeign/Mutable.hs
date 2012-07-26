{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- |
-- Module      : Data.Vector.CUDA.ForeignUnboxed.Mutable
-- Copyright   : (c) Roman Leshchinskiy 2009-2010
-- Copyright   : (c) Geoffrey Mainland 2012
-- License     : BSD-style
--
-- Maintainer  : Geoffrey Mainland <minland@apeiron.net>
-- Stability   : experimental
-- Portability : non-portable
--
-- Mutable adaptive unboxed CUDA vectors
--

module Data.Vector.CUDA.UnboxedForeign.Mutable (
  -- * Mutable vectors of primitive types
  MVector(..), IOVector, STVector, UnboxForeign,

  -- * Accessors

  -- ** Length information
  length, null,

  -- ** Extracting subvectors
  slice, init, tail, take, drop, splitAt,
  unsafeSlice, unsafeInit, unsafeTail, unsafeTake, unsafeDrop,

  -- ** Overlapping
  overlaps,

  -- * Construction

  -- ** Initialisation
  new, unsafeNew, replicate, replicateM, clone,

  -- ** Growing
  grow, unsafeGrow,

  -- ** Restricting memory usage
  clear,

  -- * Zipping and unzipping
  zip, zip3, zip4, zip5, zip6,
  unzip, unzip3, unzip4, unzip5, unzip6,

  -- * Accessing individual elements
  read, write, swap,
  unsafeRead, unsafeWrite, unsafeSwap,

  -- * Modifying vectors

  -- ** Filling and copying
  set, copy, move, unsafeCopy, unsafeMove
) where

import Data.Vector.CUDA.UnboxedForeign.Base
import qualified Data.Vector.Generic.Mutable as G
import Data.Vector.Fusion.Util ( delayed_min )
import Control.Monad.Primitive

import Prelude hiding ( length, null, replicate, reverse, map, read,
                        take, drop, splitAt, init, tail,
                        zip, zip3, unzip, unzip3 )

#include "vector.h"

-- Length information
-- ------------------

-- | Length of the mutable vector.
length :: UnboxForeign a => MVector s a -> Int
{-# INLINE length #-}
length = G.length

-- | Check whether the vector is empty
null :: UnboxForeign a => MVector s a -> Bool
{-# INLINE null #-}
null = G.null

-- Extracting subvectors
-- ---------------------

-- | Yield a part of the mutable vector without copying it.
slice :: UnboxForeign a => Int -> Int -> MVector s a -> MVector s a
{-# INLINE slice #-}
slice = G.slice

take :: UnboxForeign a => Int -> MVector s a -> MVector s a
{-# INLINE take #-}
take = G.take

drop :: UnboxForeign a => Int -> MVector s a -> MVector s a
{-# INLINE drop #-}
drop = G.drop

splitAt :: UnboxForeign a => Int -> MVector s a -> (MVector s a, MVector s a)
{-# INLINE splitAt #-}
splitAt = G.splitAt

init :: UnboxForeign a => MVector s a -> MVector s a
{-# INLINE init #-}
init = G.init

tail :: UnboxForeign a => MVector s a -> MVector s a
{-# INLINE tail #-}
tail = G.tail

-- | Yield a part of the mutable vector without copying it. No bounds checks
-- are performed.
unsafeSlice :: UnboxForeign a
            => Int  -- ^ starting index
            -> Int  -- ^ length of the slice
            -> MVector s a
            -> MVector s a
{-# INLINE unsafeSlice #-}
unsafeSlice = G.unsafeSlice

unsafeTake :: UnboxForeign a => Int -> MVector s a -> MVector s a
{-# INLINE unsafeTake #-}
unsafeTake = G.unsafeTake

unsafeDrop :: UnboxForeign a => Int -> MVector s a -> MVector s a
{-# INLINE unsafeDrop #-}
unsafeDrop = G.unsafeDrop

unsafeInit :: UnboxForeign a => MVector s a -> MVector s a
{-# INLINE unsafeInit #-}
unsafeInit = G.unsafeInit

unsafeTail :: UnboxForeign a => MVector s a -> MVector s a
{-# INLINE unsafeTail #-}
unsafeTail = G.unsafeTail

-- Overlapping
-- -----------

-- Check whether two vectors overlap.
overlaps :: UnboxForeign a => MVector s a -> MVector s a -> Bool
{-# INLINE overlaps #-}
overlaps = G.overlaps

-- Initialisation
-- --------------

-- | Create a mutable vector of the given length.
new :: (PrimMonad m, UnboxForeign a) => Int -> m (MVector (PrimState m) a)
{-# INLINE new #-}
new = G.new

-- | Create a mutable vector of the given length. The length is not checked.
unsafeNew :: (PrimMonad m, UnboxForeign a) => Int -> m (MVector (PrimState m) a)
{-# INLINE unsafeNew #-}
unsafeNew = G.unsafeNew

-- | Create a mutable vector of the given length (0 if the length is negative)
-- and fill it with an initial value.
replicate :: (PrimMonad m, UnboxForeign a) => Int -> a -> m (MVector (PrimState m) a)
{-# INLINE replicate #-}
replicate = G.replicate

-- | Create a mutable vector of the given length (0 if the length is negative)
-- and fill it with values produced by repeatedly executing the monadic action.
replicateM :: (PrimMonad m, UnboxForeign a) => Int -> m a -> m (MVector (PrimState m) a)
{-# INLINE replicateM #-}
replicateM = G.replicateM

-- | Create a copy of a mutable vector.
clone :: (PrimMonad m, UnboxForeign a)
      => MVector (PrimState m) a -> m (MVector (PrimState m) a)
{-# INLINE clone #-}
clone = G.clone

-- Growing
-- -------

-- | Grow a vector by the given number of elements. The number must be
-- positive.
grow :: (PrimMonad m, UnboxForeign a)
              => MVector (PrimState m) a -> Int -> m (MVector (PrimState m) a)
{-# INLINE grow #-}
grow = G.grow

-- | Grow a vector by the given number of elements. The number must be
-- positive but this is not checked.
unsafeGrow :: (PrimMonad m, UnboxForeign a)
               => MVector (PrimState m) a -> Int -> m (MVector (PrimState m) a)
{-# INLINE unsafeGrow #-}
unsafeGrow = G.unsafeGrow

-- Restricting memory usage
-- ------------------------

-- | Reset all elements of the vector to some undefined value, clearing all
-- references to external objects. This is usually a noop for unboxed vectors.
clear :: (PrimMonad m, UnboxForeign a) => MVector (PrimState m) a -> m ()
{-# INLINE clear #-}
clear = G.clear

-- Accessing individual elements
-- -----------------------------

-- | Yield the element at the given position.
read :: (PrimMonad m, UnboxForeign a) => MVector (PrimState m) a -> Int -> m a
{-# INLINE read #-}
read = G.read

-- | Replace the element at the given position.
write :: (PrimMonad m, UnboxForeign a) => MVector (PrimState m) a -> Int -> a -> m ()
{-# INLINE write #-}
write = G.write

-- | Swap the elements at the given positions.
swap :: (PrimMonad m, UnboxForeign a) => MVector (PrimState m) a -> Int -> Int -> m ()
{-# INLINE swap #-}
swap = G.swap


-- | Yield the element at the given position. No bounds checks are performed.
unsafeRead :: (PrimMonad m, UnboxForeign a) => MVector (PrimState m) a -> Int -> m a
{-# INLINE unsafeRead #-}
unsafeRead = G.unsafeRead

-- | Replace the element at the given position. No bounds checks are performed.
unsafeWrite
    :: (PrimMonad m, UnboxForeign a) =>  MVector (PrimState m) a -> Int -> a -> m ()
{-# INLINE unsafeWrite #-}
unsafeWrite = G.unsafeWrite

-- | Swap the elements at the given positions. No bounds checks are performed.
unsafeSwap
    :: (PrimMonad m, UnboxForeign a) => MVector (PrimState m) a -> Int -> Int -> m ()
{-# INLINE unsafeSwap #-}
unsafeSwap = G.unsafeSwap

-- Filling and copying
-- -------------------

-- | Set all elements of the vector to the given value.
set :: (PrimMonad m, UnboxForeign a) => MVector (PrimState m) a -> a -> m ()
{-# INLINE set #-}
set = G.set

-- | Copy a vector. The two vectors must have the same length and may not
-- overlap.
copy :: (PrimMonad m, UnboxForeign a)
                 => MVector (PrimState m) a -> MVector (PrimState m) a -> m ()
{-# INLINE copy #-}
copy = G.copy

-- | Copy a vector. The two vectors must have the same length and may not
-- overlap. This is not checked.
unsafeCopy :: (PrimMonad m, UnboxForeign a)
           => MVector (PrimState m) a   -- ^ target
           -> MVector (PrimState m) a   -- ^ source
           -> m ()
{-# INLINE unsafeCopy #-}
unsafeCopy = G.unsafeCopy

-- | Move the contents of a vector. The two vectors must have the same
-- length.
--
-- If the vectors do not overlap, then this is equivalent to 'copy'.
-- Otherwise, the copying is performed as if the source vector were
-- copied to a temporary vector and then the temporary vector was copied
-- to the target vector.
move :: (PrimMonad m, UnboxForeign a)
     => MVector (PrimState m) a -> MVector (PrimState m) a -> m ()
{-# INLINE move #-}
move = G.move

-- | Move the contents of a vector. The two vectors must have the same
-- length, but this is not checked.
--
-- If the vectors do not overlap, then this is equivalent to 'unsafeCopy'.
-- Otherwise, the copying is performed as if the source vector were
-- copied to a temporary vector and then the temporary vector was copied
-- to the target vector.
unsafeMove :: (PrimMonad m, UnboxForeign a)
           => MVector (PrimState m) a   -- ^ target
           -> MVector (PrimState m) a   -- ^ source
           -> m ()
{-# INLINE unsafeMove #-}
unsafeMove = G.unsafeMove

#define DEFINE_MUTABLE
#include "unbox-tuple-instances"


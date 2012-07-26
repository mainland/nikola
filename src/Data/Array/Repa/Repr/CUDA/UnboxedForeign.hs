{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : Data.Array.Repa.Repr.CUDA.UnboxedForeign
-- Copyright   : (c) Geoffrey Mainland 2012
-- License     : BSD-style
--
-- Maintainer  : Geoffrey Mainland <mainland@apeiron.net>
-- Stability   : experimental
-- Portability : non-portable

module Data.Array.Repa.Repr.CUDA.UnboxedForeign
    ( CUF
    , Array(..)

    , fromListUnboxedForeign
    , fromUnboxedForeign, toUnboxedForeign

    , fromHostArray, toHostArray

    , zip,   zip3,   zip4,   zip5,   zip6
    , unzip, unzip3, unzip4, unzip5, unzip6
    ) where

import Prelude hiding (zip, zip3, unzip, unzip3)
import Control.Monad
import Data.Array.Repa      as R
import Data.Array.Repa.Eval as R
import qualified Data.Array.Repa.Repr.UnboxedForeign     as UF
import qualified Data.Vector.CUDA.UnboxedForeign         as U
import qualified Data.Vector.CUDA.UnboxedForeign.Mutable as UM

data CUF

-- | Read elements from an unboxed vector array.
instance U.UnboxForeign a => Source CUF a where
    data Array CUF sh a = ACFUnboxed !sh !(U.Vector a)

    {-# INLINE linearIndex #-}
    linearIndex (ACFUnboxed _ vec) ix =
        vec U.! ix

    {-# INLINE unsafeLinearIndex #-}
    unsafeLinearIndex (ACFUnboxed _ vec) ix =
        vec `U.unsafeIndex` ix

    {-# INLINE extent #-}
    extent (ACFUnboxed sh _) =
        sh

    {-# INLINE deepSeqArray #-}
    deepSeqArray (ACFUnboxed sh vec) x =
        sh `deepSeq` vec `seq` x

deriving instance (Show sh, Show e, U.UnboxForeign e) => Show (Array CUF sh e)

deriving instance (Read sh, Read e, U.UnboxForeign e) => Read (Array CUF sh e)

-- | Filling of unboxed vector arrays.
instance U.UnboxForeign e => Target CUF e where
    data MVec CUF e = CUMVec (UM.IOVector e)

    {-# INLINE newMVec #-}
    newMVec n = liftM CUMVec (UM.new n)

    {-# INLINE unsafeWriteMVec #-}
    unsafeWriteMVec (CUMVec v) ix =
        UM.unsafeWrite v ix

    {-# INLINE unsafeFreezeMVec #-}
    unsafeFreezeMVec sh (CUMVec mvec) = do
        vec <- U.unsafeFreeze mvec
        return $ ACFUnboxed sh vec

    {-# INLINE deepSeqMVec #-}
    deepSeqMVec (CUMVec vec) x =
        vec `seq` x

    {-# INLINE touchMVec #-}
    touchMVec _ =
        return ()

-- | O(n). Convert a list to an unboxed vector array.
--
--   * This is an alias for `fromList` with a more specific type.
--
fromListUnboxedForeign :: (Shape sh, U.UnboxForeign a)
                       => sh -> [a] -> Array CUF sh a
{-# INLINE fromListUnboxedForeign #-}
fromListUnboxedForeign = R.fromList

-- | O(1). Wrap an unboxed vector as an array.
fromUnboxedForeign :: (Shape sh, U.UnboxForeign e)
                   => sh -> U.Vector e -> Array CUF sh e
{-# INLINE fromUnboxedForeign #-}
fromUnboxedForeign sh vec = ACFUnboxed sh vec

-- | O(1). Unpack an unboxed vector from an array.
toUnboxedForeign :: U.UnboxForeign e
                 => Array CUF sh e -> U.Vector e
{-# INLINE toUnboxedForeign #-}
toUnboxedForeign (ACFUnboxed _ vec) = vec

fromHostArray :: U.EverywhereUnboxForeign a => Array UF.UF sh a -> Array CUF sh a
fromHostArray (UF.AFUnboxed sh v) = ACFUnboxed sh (U.fromHostVector v)

toHostArray :: U.EverywhereUnboxForeign a => Array CUF sh a -> Array UF.UF sh a
toHostArray (ACFUnboxed sh v) = UF.AFUnboxed sh (U.toHostVector v)

-- Zip ------------------------------------------------------------------------
-- | O(1). Zip some unboxed arrays.
--         The shapes must be identical else `error`.
zip     :: (Shape sh, U.UnboxForeign a, U.UnboxForeign b)
        => Array CUF sh a -> Array CUF sh b
        -> Array CUF sh (a, b)
zip (ACFUnboxed sh1 vec1) (ACFUnboxed sh2 vec2)
 | sh1 /= sh2   = error "Repa: zip array shapes not identical"
 | otherwise    = ACFUnboxed sh1 (U.zip vec1 vec2)
{-# INLINE zip #-}


-- | O(1). Zip some unboxed arrays.
--         The shapes must be identical else `error`.
zip3    :: (Shape sh, U.UnboxForeign a, U.UnboxForeign b, U.UnboxForeign c)
        => Array CUF sh a -> Array CUF sh b -> Array CUF sh c
        -> Array CUF sh (a, b, c)
zip3 (ACFUnboxed sh1 vec1) (ACFUnboxed sh2 vec2) (ACFUnboxed sh3 vec3)
 | sh1 /= sh2 || sh1 /= sh3
 = error "Repa: zip array shapes not identical"
 | otherwise    = ACFUnboxed sh1 (U.zip3 vec1 vec2 vec3)
{-# INLINE zip3 #-}


-- | O(1). Zip some unboxed arrays.
--         The shapes must be identical else `error`.
zip4    :: (Shape sh, U.UnboxForeign a, U.UnboxForeign b, U.UnboxForeign c,
            U.UnboxForeign d)
        => Array CUF sh a -> Array CUF sh b -> Array CUF sh c ->
           Array CUF sh d
        -> Array CUF sh (a, b, c, d)
zip4 (ACFUnboxed sh1 vec1) (ACFUnboxed sh2 vec2) (ACFUnboxed sh3 vec3)
     (ACFUnboxed sh4 vec4)
 | sh1 /= sh2 || sh1 /= sh3 || sh1 /= sh4
 = error "Repa: zip array shapes not identical"
 | otherwise    = ACFUnboxed sh1 (U.zip4 vec1 vec2 vec3 vec4)
{-# INLINE zip4 #-}


-- | O(1). Zip some unboxed arrays.
--         The shapes must be identical else `error`.
zip5    :: (Shape sh, U.UnboxForeign a, U.UnboxForeign b, U.UnboxForeign c,
            U.UnboxForeign d, U.UnboxForeign e)
        => Array CUF sh a -> Array CUF sh b -> Array CUF sh c ->
           Array CUF sh d -> Array CUF sh e
        -> Array CUF sh (a, b, c, d, e)
zip5 (ACFUnboxed sh1 vec1) (ACFUnboxed sh2 vec2) (ACFUnboxed sh3 vec3)
     (ACFUnboxed sh4 vec4) (ACFUnboxed sh5 vec5)
 | sh1 /= sh2 || sh1 /= sh3 || sh1 /= sh4 || sh1 /= sh5
 = error "Repa: zip array shapes not identical"
 | otherwise    = ACFUnboxed sh1 (U.zip5 vec1 vec2 vec3 vec4 vec5)
{-# INLINE zip5 #-}


-- | O(1). Zip some unboxed arrays.
--         The shapes must be identical else `error`.
zip6    :: (Shape sh, U.UnboxForeign a, U.UnboxForeign b, U.UnboxForeign c,
            U.UnboxForeign d, U.UnboxForeign e, U.UnboxForeign f)
        => Array CUF sh a -> Array CUF sh b -> Array CUF sh c ->
           Array CUF sh d -> Array CUF sh e -> Array CUF sh f
        -> Array CUF sh (a, b, c, d, e, f)
zip6 (ACFUnboxed sh1 vec1) (ACFUnboxed sh2 vec2) (ACFUnboxed sh3 vec3)
     (ACFUnboxed sh4 vec4) (ACFUnboxed sh5 vec5) (ACFUnboxed sh6 vec6)
 | sh1 /= sh2 || sh1 /= sh3 || sh1 /= sh4 || sh1 /= sh5 || sh1 /= sh6
 = error "Repa: zip array shapes not identical"
 | otherwise    = ACFUnboxed sh1 (U.zip6 vec1 vec2 vec3 vec4 vec5 vec6)
{-# INLINE zip6 #-}


-- Unzip ----------------------------------------------------------------------
-- | O(1). Unzip an unboxed array.
unzip   :: (U.UnboxForeign a, U.UnboxForeign b)
        => Array CUF sh (a, b)
        -> (Array CUF sh a, Array CUF sh b)
unzip (ACFUnboxed sh vec)
 = let  (as, bs)        = U.unzip vec
   in   (ACFUnboxed sh as, ACFUnboxed sh bs)
{-# INLINE unzip #-}


-- | O(1). Unzip an unboxed array.
unzip3   :: (U.UnboxForeign a, U.UnboxForeign b, U.UnboxForeign c)
        => Array CUF sh (a, b, c)
        -> (Array CUF sh a, Array CUF sh b, Array CUF sh c)
unzip3 (ACFUnboxed sh vec)
 = let  (as, bs, cs) = U.unzip3 vec
   in   (ACFUnboxed sh as, ACFUnboxed sh bs, ACFUnboxed sh cs)
{-# INLINE unzip3 #-}


-- | O(1). Unzip an unboxed array.
unzip4   :: (U.UnboxForeign a, U.UnboxForeign b, U.UnboxForeign c, U.UnboxForeign d)
        => Array CUF sh (a, b, c, d)
        -> (Array CUF sh a, Array CUF sh b, Array CUF sh c,
            Array CUF sh d)
unzip4 (ACFUnboxed sh vec)
 = let  (as, bs, cs, ds) = U.unzip4 vec
   in   (ACFUnboxed sh as, ACFUnboxed sh bs, ACFUnboxed sh cs,
         ACFUnboxed sh ds)
{-# INLINE unzip4 #-}


-- | O(1). Unzip an unboxed array.
unzip5   :: (U.UnboxForeign a, U.UnboxForeign b, U.UnboxForeign c,
             U.UnboxForeign d, U.UnboxForeign e)
        => Array CUF sh (a, b, c, d, e)
        -> (Array CUF sh a, Array CUF sh b, Array CUF sh c,
            Array CUF sh d, Array CUF sh e)
unzip5 (ACFUnboxed sh vec)
 = let  (as, bs, cs, ds, es) = U.unzip5 vec
   in   (ACFUnboxed sh as, ACFUnboxed sh bs, ACFUnboxed sh cs,
         ACFUnboxed sh ds, ACFUnboxed sh es)
{-# INLINE unzip5 #-}


-- | O(1). Unzip an unboxed array.
unzip6  :: (U.UnboxForeign a, U.UnboxForeign b, U.UnboxForeign c,
            U.UnboxForeign d, U.UnboxForeign e, U.UnboxForeign f)
        => Array CUF sh (a, b, c, d, e, f)
        -> (Array CUF sh a, Array CUF sh b, Array CUF sh c,
            Array CUF sh d, Array CUF sh e, Array CUF sh f)
unzip6 (ACFUnboxed sh vec)
 = let  (as, bs, cs, ds, es, fs) = U.unzip6 vec
   in   (ACFUnboxed sh as, ACFUnboxed sh bs, ACFUnboxed sh cs,
         ACFUnboxed sh ds, ACFUnboxed sh es, ACFUnboxed sh fs)
{-# INLINE unzip6 #-}

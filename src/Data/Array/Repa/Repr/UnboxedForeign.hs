-- |
-- Module      : Data.Array.Repa.Repr.UnboxedForeign
-- Copyright   : (c) Geoffrey Mainland 2012
-- License     : BSD-style
--
-- Maintainer  : Geoffrey Mainland <mainland@apeiron.net>
-- Stability   : experimental
-- Portability : non-portable

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Array.Repa.Repr.UnboxedForeign
    ( UF
    , Array(..)
    , fromListUnboxedForeign
    , fromUnboxedForeign, toUnboxedForeign

    , zip,   zip3,   zip4,   zip5,   zip6
    , unzip, unzip3, unzip4, unzip5, unzip6
    ) where

import Prelude hiding (zip, zip3, unzip, unzip3)
import Control.Monad
import Data.Array.Repa      as R
import Data.Array.Repa.Eval as R
import qualified Data.Vector.UnboxedForeign         as U
import qualified Data.Vector.UnboxedForeign.Mutable as UM

data UF

-- | Read elements from an unboxed vector array.
instance U.UnboxForeign a => Source UF a where
    data Array UF sh a = AFUnboxed !sh !(U.Vector a)

    {-# INLINE linearIndex #-}
    linearIndex (AFUnboxed _ vec) ix =
        vec U.! ix

    {-# INLINE unsafeLinearIndex #-}
    unsafeLinearIndex (AFUnboxed _ vec) ix =
        vec `U.unsafeIndex` ix

    {-# INLINE extent #-}
    extent (AFUnboxed sh _) =
        sh

    {-# INLINE deepSeqArray #-}
    deepSeqArray (AFUnboxed sh vec) x =
        sh `deepSeq` vec `seq` x

deriving instance (Show sh, Show e, U.UnboxForeign e) => Show (Array UF sh e)

deriving instance (Read sh, Read e, U.UnboxForeign e) => Read (Array UF sh e)

-- | Filling of unboxed vector arrays.
instance U.UnboxForeign e => Target UF e where
    data MVec UF e = CUMVec (UM.IOVector e)

    {-# INLINE newMVec #-}
    newMVec n = liftM CUMVec (UM.new n)

    {-# INLINE unsafeWriteMVec #-}
    unsafeWriteMVec (CUMVec v) ix =
        UM.unsafeWrite v ix

    {-# INLINE unsafeFreezeMVec #-}
    unsafeFreezeMVec sh (CUMVec mvec) = do
        vec <- U.unsafeFreeze mvec
        return $ AFUnboxed sh vec

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
                       => sh -> [a] -> Array UF sh a
{-# INLINE fromListUnboxedForeign #-}
fromListUnboxedForeign = R.fromList

-- | O(1). Wrap an unboxed vector as an array.
fromUnboxedForeign :: (Shape sh, U.UnboxForeign e)
                   => sh -> U.Vector e -> Array UF sh e
{-# INLINE fromUnboxedForeign #-}
fromUnboxedForeign sh vec = AFUnboxed sh vec

-- | O(1). Unpack an unboxed vector from an array.
toUnboxedForeign :: U.UnboxForeign e
                 => Array UF sh e -> U.Vector e
{-# INLINE toUnboxedForeign #-}
toUnboxedForeign (AFUnboxed _ vec) = vec


-- Zip ------------------------------------------------------------------------
-- | O(1). Zip some unboxed arrays.
--         The shapes must be identical else `error`.
zip     :: (Shape sh, U.UnboxForeign a, U.UnboxForeign b)
        => Array UF sh a -> Array UF sh b
        -> Array UF sh (a, b)
zip (AFUnboxed sh1 vec1) (AFUnboxed sh2 vec2)
 | sh1 /= sh2   = error "Repa: zip array shapes not identical"
 | otherwise    = AFUnboxed sh1 (U.zip vec1 vec2)
{-# INLINE zip #-}


-- | O(1). Zip some unboxed arrays.
--         The shapes must be identical else `error`.
zip3    :: (Shape sh, U.UnboxForeign a, U.UnboxForeign b, U.UnboxForeign c)
        => Array UF sh a -> Array UF sh b -> Array UF sh c
        -> Array UF sh (a, b, c)
zip3 (AFUnboxed sh1 vec1) (AFUnboxed sh2 vec2) (AFUnboxed sh3 vec3)
 | sh1 /= sh2 || sh1 /= sh3
 = error "Repa: zip array shapes not identical"
 | otherwise    = AFUnboxed sh1 (U.zip3 vec1 vec2 vec3)
{-# INLINE zip3 #-}


-- | O(1). Zip some unboxed arrays.
--         The shapes must be identical else `error`.
zip4    :: (Shape sh, U.UnboxForeign a, U.UnboxForeign b, U.UnboxForeign c,
            U.UnboxForeign d)
        => Array UF sh a -> Array UF sh b -> Array UF sh c ->
           Array UF sh d
        -> Array UF sh (a, b, c, d)
zip4 (AFUnboxed sh1 vec1) (AFUnboxed sh2 vec2) (AFUnboxed sh3 vec3)
     (AFUnboxed sh4 vec4)
 | sh1 /= sh2 || sh1 /= sh3 || sh1 /= sh4
 = error "Repa: zip array shapes not identical"
 | otherwise    = AFUnboxed sh1 (U.zip4 vec1 vec2 vec3 vec4)
{-# INLINE zip4 #-}


-- | O(1). Zip some unboxed arrays.
--         The shapes must be identical else `error`.
zip5    :: (Shape sh, U.UnboxForeign a, U.UnboxForeign b, U.UnboxForeign c,
            U.UnboxForeign d, U.UnboxForeign e)
        => Array UF sh a -> Array UF sh b -> Array UF sh c ->
           Array UF sh d -> Array UF sh e
        -> Array UF sh (a, b, c, d, e)
zip5 (AFUnboxed sh1 vec1) (AFUnboxed sh2 vec2) (AFUnboxed sh3 vec3)
     (AFUnboxed sh4 vec4) (AFUnboxed sh5 vec5)
 | sh1 /= sh2 || sh1 /= sh3 || sh1 /= sh4 || sh1 /= sh5
 = error "Repa: zip array shapes not identical"
 | otherwise    = AFUnboxed sh1 (U.zip5 vec1 vec2 vec3 vec4 vec5)
{-# INLINE zip5 #-}


-- | O(1). Zip some unboxed arrays.
--         The shapes must be identical else `error`.
zip6    :: (Shape sh, U.UnboxForeign a, U.UnboxForeign b, U.UnboxForeign c,
            U.UnboxForeign d, U.UnboxForeign e, U.UnboxForeign f)
        => Array UF sh a -> Array UF sh b -> Array UF sh c ->
           Array UF sh d -> Array UF sh e -> Array UF sh f
        -> Array UF sh (a, b, c, d, e, f)
zip6 (AFUnboxed sh1 vec1) (AFUnboxed sh2 vec2) (AFUnboxed sh3 vec3)
     (AFUnboxed sh4 vec4) (AFUnboxed sh5 vec5) (AFUnboxed sh6 vec6)
 | sh1 /= sh2 || sh1 /= sh3 || sh1 /= sh4 || sh1 /= sh5 || sh1 /= sh6
 = error "Repa: zip array shapes not identical"
 | otherwise    = AFUnboxed sh1 (U.zip6 vec1 vec2 vec3 vec4 vec5 vec6)
{-# INLINE zip6 #-}


-- Unzip ----------------------------------------------------------------------
-- | O(1). Unzip an unboxed array.
unzip   :: (U.UnboxForeign a, U.UnboxForeign b)
        => Array UF sh (a, b)
        -> (Array UF sh a, Array UF sh b)
unzip (AFUnboxed sh vec)
 = let  (as, bs)        = U.unzip vec
   in   (AFUnboxed sh as, AFUnboxed sh bs)
{-# INLINE unzip #-}


-- | O(1). Unzip an unboxed array.
unzip3   :: (U.UnboxForeign a, U.UnboxForeign b, U.UnboxForeign c)
        => Array UF sh (a, b, c)
        -> (Array UF sh a, Array UF sh b, Array UF sh c)
unzip3 (AFUnboxed sh vec)
 = let  (as, bs, cs) = U.unzip3 vec
   in   (AFUnboxed sh as, AFUnboxed sh bs, AFUnboxed sh cs)
{-# INLINE unzip3 #-}


-- | O(1). Unzip an unboxed array.
unzip4   :: (U.UnboxForeign a, U.UnboxForeign b, U.UnboxForeign c, U.UnboxForeign d)
        => Array UF sh (a, b, c, d)
        -> (Array UF sh a, Array UF sh b, Array UF sh c,
            Array UF sh d)
unzip4 (AFUnboxed sh vec)
 = let  (as, bs, cs, ds) = U.unzip4 vec
   in   (AFUnboxed sh as, AFUnboxed sh bs, AFUnboxed sh cs,
         AFUnboxed sh ds)
{-# INLINE unzip4 #-}


-- | O(1). Unzip an unboxed array.
unzip5   :: (U.UnboxForeign a, U.UnboxForeign b, U.UnboxForeign c,
             U.UnboxForeign d, U.UnboxForeign e)
        => Array UF sh (a, b, c, d, e)
        -> (Array UF sh a, Array UF sh b, Array UF sh c,
            Array UF sh d, Array UF sh e)
unzip5 (AFUnboxed sh vec)
 = let  (as, bs, cs, ds, es) = U.unzip5 vec
   in   (AFUnboxed sh as, AFUnboxed sh bs, AFUnboxed sh cs,
         AFUnboxed sh ds, AFUnboxed sh es)
{-# INLINE unzip5 #-}


-- | O(1). Unzip an unboxed array.
unzip6  :: (U.UnboxForeign a, U.UnboxForeign b, U.UnboxForeign c,
            U.UnboxForeign d, U.UnboxForeign e, U.UnboxForeign f)
        => Array UF sh (a, b, c, d, e, f)
        -> (Array UF sh a, Array UF sh b, Array UF sh c,
            Array UF sh d, Array UF sh e, Array UF sh f)
unzip6 (AFUnboxed sh vec)
 = let  (as, bs, cs, ds, es, fs) = U.unzip6 vec
   in   (AFUnboxed sh as, AFUnboxed sh bs, AFUnboxed sh cs,
         AFUnboxed sh ds, AFUnboxed sh es, AFUnboxed sh fs)
{-# INLINE unzip6 #-}

{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Data.Array.Nikola.Util.Random
-- Copyright   : (c) The President and Fellows of Harvard College 2009-2010
-- Copyright   : (c) Geoffrey Mainland 2012
-- License     : BSD-style
--
-- Maintainer  : Geoffrey Mainland <mainland@apeiron.net>
-- Stability   : experimental
-- Portability : non-portable

module Data.Array.Nikola.Util.Random (
    PureMTRandom(..),
    randoms,
    randomsRange
  ) where

import Data.Int
import qualified Data.Vector.Fusion.Stream         as Stream
import qualified Data.Vector.Fusion.Stream.Monadic as S
import qualified Data.Vector.Fusion.Stream.Size    as S
import qualified Data.Vector.Generic as G
import Data.Vector.Fusion.Util
import qualified System.Random.Mersenne.Pure64 as R
import GHC.Float

randoms :: forall v a . (G.Vector v a, PureMTRandom a)
        => Int
        -> IO (v a)
randoms n = do
    g <- R.newPureMT
    return $ G.unstream (randomS g n)
  where
    randomS :: PureMTRandom a => R.PureMT -> Int -> Stream.Stream a
    {-# INLINE [1] randomS #-}
    randomS g n =
        S.Stream (return . step) (n, g) (S.Exact (delay_inline max n 0))
      where
        {-# INLINE [0] step #-}
        step (i,g) | i <= 0    = S.Done
                   | otherwise = g `seq` case random g of
                                    (r, g') -> S.Yield r (i-1, g')


randomsRange :: forall v a . (Fractional a, G.Vector v a, PureMTRandom a)
             => Int
             -> (a, a)
             -> IO (v a)
randomsRange n (lo, hi) = do
    g <- R.newPureMT
    return $ G.unstream (randomS g n)
  where
    scale :: Int -> a
    scale x = (1.0 - t) * lo + t * hi
      where
        t :: a
        t = fromIntegral x / fromIntegral (maxBound :: Int)

    randomS :: R.PureMT -> Int -> Stream.Stream a
    {-# INLINE [1] randomS #-}
    randomS g n = S.Stream (return . step) (n, g) (S.Exact (delay_inline max n 0))
      where
        {-# INLINE [0] step #-}
        step (i,g) | i <= 0    = S.Done
                   | otherwise = g `seq` case random g of
                                    (r, g') -> S.Yield (scale r) (i-1, g')

class PureMTRandom a where
    random :: R.PureMT -> (a, R.PureMT)

instance PureMTRandom Int where
    random = R.randomInt

instance PureMTRandom Int32 where
    random g = let (i, g') = R.randomInt g
               in
                 (fromIntegral i, g')

instance PureMTRandom Double where
    random = R.randomDouble

instance PureMTRandom Float where
    random g = let (f, g') = R.randomDouble g
               in
                 (double2Float f, g')

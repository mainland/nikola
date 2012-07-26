{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : Data.Array.Nikola.Operators.Mapping
-- Copyright   : (c) Geoffrey Mainland 2012
-- License     : BSD-style
--
-- Maintainer  : Geoffrey Mainland <mainland@apeiron.net>
-- Stability   : experimental
-- Portability : non-portable

module Data.Array.Nikola.Operators.Mapping (
    Map(..),
    map,
    mapP,

    Zip(..),
    zip,

    ZipWith(..),
    zipWith,

    ZipWith3(..),
    zipWith3
  ) where

import Prelude hiding (map, zip, zipWith, zipWith3)

import Data.Array.Nikola.Array
import Data.Array.Nikola.Repr.Delayed
import Data.Array.Nikola.Repr.Push
import Data.Array.Nikola.Shape

import Data.Array.Nikola.Language.Monad

-- | Map
class (IsArray r1 a, IsArray r2 a) => Map r1 r2 a where
    map_ :: Shape sh => (a -> b) -> Array r1 sh a -> Array r2 sh b

instance Source r a => Map r D a where
    map_ f arr =
        ADelayed sh (f . g)
      where
        ADelayed sh g = delay arr

instance Map PSH PSH a where
    map_ (f :: a -> b) (APush (sh :: sh) m) = APush sh m'
      where
        m' :: P (sh, b)
        m' = do  (i, x) <- m
                 return (i, f x)

map :: (Shape sh, Map r D a)
    => (a -> b) -> Array r sh a -> Array D sh b
map = map_

mapP :: (Shape sh, Map r PSH a)
     => (a -> b) -> Array r sh a -> Array PSH sh b
mapP = map_

-- | Zip
class (IsArray r1 a, IsArray r2 b, IsArray r3 (a, b)) => Zip r1 r2 r3 a b where
    zip_ :: Shape sh => Array r1 sh a -> Array r2 sh b -> Array r3 sh (a, b)

instance (Source r1 a, Source r2 b) => Zip r1 r2 D a b where
    zip_ xs ys =
        ADelayed (intersectDim sh1 sh2) (\i -> (f i, g i))
      where
        ADelayed sh1 f = delay xs
        ADelayed sh2 g = delay ys

zip :: (Shape sh, Zip r1 r2 D a b)
    => Array r1 sh a -> Array r2 sh b -> Array D sh (a, b)
zip = zip_

-- | ZipWith
class (IsArray r1 a, IsArray r2 b, IsArray r3 c) => ZipWith r1 r2 r3 a b c where
    zipWith_ :: Shape sh
             => (a -> b -> c)
             -> Array r1 sh a
             -> Array r2 sh b
             -> Array r3 sh c

instance (Source r1 a, Source r2 b) => ZipWith r1 r2 D a b c where
    zipWith_ (f :: a -> b -> c) xs ys =
        ADelayed (intersectDim sh1 sh2) (\i -> f (g i) (h i))
      where
        ADelayed sh1 g = delay xs
        ADelayed sh2 h = delay ys

zipWith :: (Shape sh, ZipWith r1 r2 D a b c)
        => (a -> b -> c)
        -> Array r1 sh a
        -> Array r2 sh b
        -> Array D sh c
zipWith = zipWith_

-- | ZipWith3
class (IsArray r1 a, IsArray r2 b, IsArray r3 c, IsArray r4 d) => ZipWith3 r1 r2 r3 r4 a b c d where
    zipWith3_ :: Shape sh
              => (a -> b -> c -> d)
              -> Array r1 sh a
              -> Array r2 sh b
              -> Array r3 sh c
              -> Array r4 sh d

instance (Source r1 a, Source r2 b, Source r3 c) => ZipWith3 r1 r2 r3 D a b c d where
    zipWith3_ (f :: a -> b -> c -> d) xs ys zs =
        ADelayed (intersectDim (intersectDim sh1 sh2) sh3) (\i -> f (g i) (h i) (h' i))
      where
        ADelayed sh1 g  = delay xs
        ADelayed sh2 h  = delay ys
        ADelayed sh3 h' = delay zs

zipWith3 :: (Shape sh, ZipWith3 r1 r2 r3 D a b c d)
         => (a -> b -> c -> d)
         -> Array r1 sh a
         -> Array r2 sh b
         -> Array r3 sh c
         -> Array D sh d
zipWith3 = zipWith3_

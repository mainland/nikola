{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : Data.Array.Nikola.Util.Generic
-- Copyright   : (c) Geoffrey Mainland 2012
-- License     : BSD-style
--
-- Maintainer  : Geoffrey Mainland <mainland@apeiron.net>
-- Stability   : experimental
-- Portability : non-portable

module Data.Array.Nikola.Util.Generic (
    Traversal,
    TraversableFamily(..),

    Fold,
    foldFam
  ) where

import Control.Applicative (Applicative)
import Data.Functor.Constant
import Data.Monoid

type Traversal fam f = forall a. fam a -> a -> f a

class TraversableFamily fam where
    traverseFam :: Applicative f => Traversal fam f -> Traversal fam f

type Fold fam m = forall a. fam a -> a -> m

foldFam :: (TraversableFamily fam, Monoid m) => Fold fam m -> Fold fam m
foldFam child w a = getConstant $ traverseFam (\v b -> Constant $ child v b) w a

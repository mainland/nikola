{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : Data.Array.Nikola.Operators.Reduction
-- Copyright   : (c) Geoffrey Mainland 2012
-- License     : BSD-style
--
-- Maintainer  : Geoffrey Mainland <mainland@apeiron.net>
-- Stability   : experimental
-- Portability : non-portable

module Data.Array.Nikola.Operators.Reduction (
    --scan,
    --prescan
  ) where

{-
import Data.Array.Nikola.Array
import Data.Array.Nikola.Exp
import Data.Array.Nikola.Repr.Delayed
import Data.Array.Nikola.Repr.Manifest
import Data.Array.Nikola.Shape

import Data.Array.Nikola.Language.Monad
import Data.Array.Nikola.Language.Syntax hiding (Exp, Var)

scan :: (Shape sh t,
         IsScalar t a,
         IsArray  r t (Exp t a),
         Manifest r t (Exp t a))
     => (Exp t a -> Exp t a -> Exp t a)
     -> Array r (sh t) (Exp t a)
     -> P (Array M (sh t) (Exp t a))
scan f arr = do
    x <- gensym "x"
    y <- gensym "y"
    (AManifest _ vsrc)     <- mkManifest arr
    dst@(AManifest _ vdst) <- alloca (extent arr)
    shift $ \k -> do
    p <- reset $ k dst
    return $ ScanK x y (unE $ f (varE (V x)) (varE (V y))) vsrc vdst `seqK` p

prescan :: (Shape sh t,
            IsScalar t a,
            IsArray  r t (Exp t a),
            Manifest r t (Exp t a))
        => Exp t a
        -> (Exp t a -> Exp t a -> Exp t a)
        -> Array r (sh t) (Exp t a)
        -> P (Array M (sh t) (Exp t a))
prescan z f arr = do
    x <- gensym "x"
    y <- gensym "y"
    (AManifest _ vsrc)     <- mkManifest arr
    dst@(AManifest _ vdst) <- alloca (extent arr)
    shift $ \k -> do
    p <- reset $ k dst
    return $ PrescanK (unE z) x y (unE $ f (varE (V x)) (varE (V y))) vsrc vdst `seqK` p
-}

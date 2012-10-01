{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : Data.Array.Nikola.Combinators
-- Copyright   : (c) Geoffrey Mainland 2012
-- License     : BSD-style
--
-- Maintainer  : Geoffrey Mainland <mainland@apeiron.net>
-- Stability   : experimental
-- Portability : non-portable

module Data.Array.Nikola.Combinators (
    iterate
  ) where

import Prelude hiding (iterate)

import Data.Int

import Data.Array.Nikola.Exp
import Data.Array.Nikola.Language.Reify
import Data.Array.Nikola.Language.Syntax hiding (Exp, Var)

-- | 'iterate n f x' iterates the function 'f' 'n' times with the initial value
-- 'x'. GHC cannot in general tell that the 't' in 'Exp t Int32' is the same 't'
-- that appears in the lifted type of 'a', so a type signature on 'n' may be
-- necessary to disambiguate.
iterate :: forall t a .
           ( IsElem (Exp t (Lifted t a))
           , Lift t a
           , Unlift t (Lifted t a)
           , a ~ Unlifted t (Lifted t a))
        => Exp t Int32 -> (a -> a) -> a -> a
iterate n f x =
    unlift e
  where
    x' :: Exp t (Lifted t a)
    x' = lift x

    f' :: Exp t (Lifted t a) -> Exp t (Lifted t a)
    f' = lift . f . unlift

    e :: Exp t (Lifted t a)
    e = E $ IterateE (unE n) (delayE f') (unE x')

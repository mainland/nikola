{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : Data.Array.Nikola.Repr.Delayed
-- Copyright   : (c) Geoffrey Mainland 2012
-- License     : BSD-style
--
-- Maintainer  : Geoffrey Mainland <mainland@apeiron.net>
-- Stability   : experimental
-- Portability : non-portable

module Data.Array.Nikola.Repr.Delayed (
    D,
    Array(..),

    fromFunction,
    toFunction,

    delay
  ) where

import Data.Typeable (Typeable)

import Data.Array.Nikola.Array
import Data.Array.Nikola.Exp
import Data.Array.Nikola.Repr.Manifest
import Data.Array.Nikola.Shape

import Data.Array.Nikola.Language.Monad
import Data.Array.Nikola.Language.Syntax hiding (Var, Exp)

-- | "Delayed" arrays represented by a function from index to element
data D
  deriving (Typeable)

instance IsArray D a where
    data Array D sh a = ADelayed sh (sh -> a)

    extent (ADelayed sh _) = sh

instance Source D a where
    index (ADelayed _ f) ix = f ix

    linearIndex (ADelayed sh f) ix = f (fromIndex sh ix)

instance IsElem a => Manifest D a where
    manifest (ADelayed sh f) v = do
    p1 <- reset $ do  i <- parfor sh
                      write v i (f i)
                      return $ ReturnK UnitE
    shift $ \k -> do
    p2 <- reset $ k ()
    return $ p1 `seqK` p2

-- | Construct a delayed array from a function mapping indices to values.
fromFunction :: forall sh a . (Shape sh)
             => sh
             -> (sh -> a)
             -> Array D sh a
fromFunction sh f = ADelayed sh f

-- | Produce the extent of an array and a function to retrieve an arbitrary
-- element.
toFunction :: (Shape sh, Source r a)
           => Array r sh a -> (sh, sh -> a)
toFunction arr =
    case delay arr of
      ADelayed sh f -> (sh, f)

-- | Convert an array into a delayed array
delay :: (Shape sh, Source r e)
      => Array r sh e
      -> Array D sh e
delay arr = ADelayed (extent arr) (unsafeIndex arr)

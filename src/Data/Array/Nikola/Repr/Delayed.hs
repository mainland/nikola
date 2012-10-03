{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
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
    generate,
    toFunction,

    delay
  ) where

import Data.Typeable (Typeable)

import Data.Array.Nikola.Array
import Data.Array.Nikola.Exp
import Data.Array.Nikola.Eval
import Data.Array.Nikola.Program
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

instance Shape sh => Load D sh e where
    loadP (ADelayed sh f) marr = do
        p1 <- reset $ do  i <- parfor sh
                          unsafeWriteMArray marr i (f i)
                          return $ ReturnE UnitE
        seqK p1 ()

-- | Construct a delayed array from a function mapping indices to values.
fromFunction :: forall sh a . (Shape sh)
             => sh
             -> (sh -> a)
             -> Array D sh a
fromFunction sh f = ADelayed sh f

-- | A version of 'fromFunction' specialized to 'Vector's
generate :: IsElem (Exp t Ix) => Exp t Ix -> (Exp t Ix -> a) -> Array D (DIM1 t) a
generate n f = fromFunction (ix1 n) (\(Z:.i) -> f i)

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

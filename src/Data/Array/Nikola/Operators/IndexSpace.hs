{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module      : Data.Array.Nikola.Operators.IndexSpace
-- Copyright   : (c) Geoffrey Mainland 2012
-- License     : BSD-style
--
-- Maintainer  : Geoffrey Mainland <mainland@apeiron.net>
-- Stability   : experimental
-- Portability : non-portable

module Data.Array.Nikola.Operators.IndexSpace (
    reshape,

    Append(..),
    append,
    appendP,
    (++),

    Reverse(..),
    reverse,
    reverseP,

    enumFromN,
    enumFromStepN,

    replicate,

    init,
    tail,

    take,
    drop
  ) where

import Prelude hiding ((++), drop, init, map, max, min, replicate, reverse, tail, take)
import qualified Prelude as P
import Data.Typeable (Typeable)

import Data.Array.Nikola.Array
import Data.Array.Nikola.Exp
import Data.Array.Nikola.Program
import Data.Array.Nikola.Repr.Delayed
import Data.Array.Nikola.Repr.Push
import Data.Array.Nikola.Shape

import Data.Array.Nikola.Language.Monad
import Data.Array.Nikola.Language.Syntax hiding (Exp, Var)

reshape :: (Shape sh1, Shape sh2, Source r a)
        => sh2
        -> Array r sh1 a
        -> Array D sh2 a
-- XXX: must dynamically check that size sh2 == size (extent arr)
reshape sh2 arr =
    fromFunction sh2 $
    unsafeIndex arr . fromIndex (extent arr) . toIndex sh2

-- | Reverse
class (IsArray r1 a, IsArray r2 a) => Reverse r1 r2 t a where
    reverse_ :: Array r1 (DIM1 t) a -> Array r2 (DIM1 t) a

instance (Source r a, IsElem (Exp t Ix)) => Reverse r D t a where
    reverse_ arr = ADelayed sh g
      where
        ADelayed sh@(Z:.n) f = delay arr

        g :: DIM1 t -> a
        g (Z:.i) = f (Z:.(n-i-1))

instance IsElem (Exp t Ix) => Reverse PSH PSH t a where
    reverse_ (APush sh@(Z:.n) m) = APush sh m'
      where
        m' :: P (DIM1 t, a)
        m' = do  (Z:.i,x) <- m
                 return (Z:.(n-i-1),x)

reverse :: (Reverse r D t a)
        => Array r (DIM1 t) a
        -> Array D (DIM1 t) a
reverse = reverse_

reverseP :: (Reverse r PSH t a)
         => Array r (DIM1 t) a
         -> Array PSH (DIM1 t) a
reverseP = reverse_

-- | Append
class (IsArray r1 a,
       IsArray r2 a,
       IsArray r3 a)
    => Append r1 r2 r3 t a where
    append_ :: Array r1 (DIM1 t) a -> Array r2 (DIM1 t) a -> Array r3 (DIM1 t) a

instance (Typeable t,
          IsElem (Exp t Ix),
          Source r1 a,
          Source r2 a,
          IfThenElse (Exp t Bool) a)
    => Append r1 r2 D t a where
    append_ arr1 arr2 =
        ADelayed (ix1 (n1+n2)) h
      where
        ADelayed (Z:.n1) h1 = delay arr1
        ADelayed (Z:.n2) h2 = delay arr2

        h :: DIM1 t -> a
        h (Z:.i) = if (i <* n1)
                     then h1 (ix1 i)
                     else h2 (ix1 (i-n1))

instance Append PSH PSH PSH t a where
   append_ (APush (Z:.n1) m1) (APush (Z:.n2) m2) =
       APush (ix1 (n1+n2)) m'
      where
        m' :: P (DIM1 t, a)
        m' = shift $ \k -> do
            p1 <- reset $ m1 >>= \(Z:.i,x) -> k (Z:.i,x)
            p2 <- reset $ m2 >>= \(Z:.i,x) -> k (Z:.i+n1,x)
            return $ p1 `parE` p2

append :: (Append r1 r2 D t a)
       => Array r1 (DIM1 t) a
       -> Array r2 (DIM1 t) a
       -> Array D (DIM1 t) a
append = append_

(++) :: (Append r1 r2 D t a)
     => Array r1 (DIM1 t) a
     -> Array r2 (DIM1 t) a
     -> Array D (DIM1 t) a
(++) = append_

appendP :: (Append r1 r2 PSH t a)
        => Array r1 (DIM1 t) a
        -> Array r2 (DIM1 t) a
        -> Array PSH (DIM1 t) a
appendP = append_

enumFromN :: ( Typeable t
             , Shape sh
             , IsNum t a
             , IsNum t Ix
             , IsElem (Exp t a)
             , IsElem (Exp t Ix)
             , Num (Exp t a)
             )
          => sh
          -> Exp t a
          -> Array D sh (Exp t a)
enumFromN sh x = enumFromStepN sh x 1

enumFromStepN :: forall t sh a .
                 ( Typeable t
                 , Shape sh
                 , IsElem (Exp t Ix)
                 , IsElem (Exp t a)
                 , Num (Exp t a)
                 , IsNum t a
                 )
              => sh
              -> Exp t a
              -> Exp t a
              -> Array D sh (Exp t a)
enumFromStepN sh x y =
    reshape sh arr'
  where
    arr' :: Array D (DIM1 t) (Exp t a)
    arr'= fromFunction (ix1 1) (\(Z:.i) -> x + fromInt i*y)

replicate :: (Typeable t, IsElem (Exp t Ix))
          => Exp t Ix -> a -> Array D (DIM1 t) a
replicate n x =
    fromFunction (ix1 n) (const x)

init :: forall r t a . (Typeable t, IsElem (Exp t Ix), Source r a)
     => Array r (DIM1 t) a -> Array D (DIM1 t) a
init arr = fromFunction (ix1 sz') (index arr)
  where
    sz' :: Exp t Ix
    sz' = max 0 (sz-1)

    sz :: Exp t Ix
    !(Z:.sz) = extent arr

tail :: forall r t a . (Typeable t, IsElem (Exp t Ix), Source r a)
     => Array r (DIM1 t) a -> Array D (DIM1 t) a
tail arr = fromFunction (ix1 sz') (\(Z:.i) -> index arr (ix1 (i+1)))
  where
    sz' :: Exp t Ix
    sz' = max 0 (sz-1)

    sz :: Exp t Ix
    !(Z:.sz) = extent arr

take :: forall r t a . (Typeable t, IsElem (Exp t Ix), Source r a)
     => Exp t Ix -> Array r (DIM1 t) a -> Array D (DIM1 t) a
take n arr = fromFunction (ix1 sz') (index arr)
  where
    sz' :: Exp t Ix
    sz' = if n <* 0 then 0 else min n sz

    sz :: Exp t Ix
    !(Z:.sz) = extent arr

drop :: forall r t a . (Typeable t, IsElem (Exp t Ix), Source r a)
     => Exp t Ix -> Array r (DIM1 t) a -> Array D (DIM1 t) a
drop n arr = fromFunction (ix1 sz') (\(Z:.i) -> index arr (ix1 (if n <* 0 then i else i+n)))
  where
    sz' :: Exp t Ix
    sz' = if n <* 0 then sz else max 0 (sz-n)

    sz :: Exp t Ix
    !(Z:.sz) = extent arr

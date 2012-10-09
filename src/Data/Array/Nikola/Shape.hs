{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : Data.Array.Nikola.Shape
-- Copyright   : (c) Geoffrey Mainland 2012
-- License     : BSD-style
--
-- Maintainer  : Geoffrey Mainland <mainland@apeiron.net>
-- Stability   : experimental
-- Portability : non-portable

module Data.Array.Nikola.Shape (
    Ix,
    Shape(..),
    Z(..),
    (:.)(..),

    Rsh,

    DIM0,
    DIM1,
    DIM2,

    ix1, ix2
  ) where

import Data.Typeable (Typeable)

import qualified Data.Array.Repa as R

import Data.Array.Nikola.Exp
import Data.Array.Nikola.Language.Check (extendVarTypes)
import Data.Array.Nikola.Language.Monad
import Data.Array.Nikola.Language.Syntax hiding (Exp, Var)

-- | Array shapes
class (Typeable sh) => Shape sh where
    -- | Get the number of dimensions in a shape.
    rank :: sh -> Int

    -- | The shape of an array of size zero, with a particular dimensionality.
    zeroDim :: sh

    -- | The shape of an array with size one, with a particular dimensionality.
    unitDim :: sh

    -- | Compute the intersection of two shapes.
    intersectDim :: sh -> sh -> sh

    -- | Add the coordinates of two shapes componentwise
    addDim :: sh -> sh -> sh

    -- | Get the total number of elements in an array with this shape.
    size :: sh -> Exp t Ix

    -- | Convert an index into its equivalent flat, linear, row-major version.
    toIndex :: sh  -- ^ Shape of the array.
            -> sh  -- ^ Index into the array.
            -> Exp t Ix

    -- | Inverse of `toIndex`.
    fromIndex :: sh      -- ^ Shape of the array.
              -> Exp t Ix -- ^ Index into linear representation.
              -> sh

    -- | Convert a shape into its list of dimensions.
    listOfShape :: sh -> [Exp t Ix]

    -- | Convert a list of dimensions to a shape
    shapeOfList :: [Exp t Ix] -> sh

    -- | Shape-polymorphic for
    for :: sh -> P sh

    -- | Shape-polymorphic parallel for
    parfor :: sh -> P sh

-- | An index of dimension zero
data Z = Z
  deriving (Show, Read, Eq, Ord, Typeable)

instance Shape Z where
    rank _ = 0

    zeroDim = Z

    unitDim = Z

    intersectDim _ _ = Z

    addDim _ _ = Z

    size _ = 1

    toIndex _ _ = 0

    fromIndex _ _ = Z

    listOfShape _ = []

    shapeOfList [] = Z
    shapeOfList _  = error "shapeOfList: non-empty list when converting to Z"

    for _ = return Z

    parfor _ = return Z

-- | Our index type, used for both shapes and indices.
infixl 3 :.

data (:.) tail head = tail :. head
  deriving (Typeable)

coerceT :: Exp t1 a -> Exp t2 a
coerceT (E e) = E e

instance (Shape sh, IsElem (Exp t Ix)) => Shape (sh :. Exp t Ix) where
    rank _ = rank (undefined :: sh) + 1

    zeroDim = zeroDim :. 0

    unitDim = unitDim :. 1

    intersectDim (sh1 :. n1) (sh2 :. n2) =
        intersectDim sh1 sh2 :. E (BinopE MinO (unE n1) (unE n2))

    addDim (sh1 :. n1) (sh2 :. n2) =
        addDim sh1 sh2 :. (n1 + n2)

    size (sh1 :. n) =
        size sh1 * coerceT n

    toIndex (sh1 :. sh2) (sh1' :. sh2') =
        toIndex sh1 sh1' * coerceT sh2 + coerceT sh2'

    fromIndex (ds :. d) n =
        fromIndex ds (n `quot` coerceT d) :. r
      where
        r = coerceT n `rem` coerceT d

    listOfShape (sh :. n) =
        coerceT n : listOfShape sh

    shapeOfList []     = error "shapeOfList: empty list when converting to  (_ :. Exp t Ix)"
    shapeOfList (x:xs) = shapeOfList xs :. coerceT x

    for (sh:.(n :: Exp t Ix)) = do
        is <- for sh
        v  <- gensym "i"
        shift $ \k -> do
        p <- reset $ extendVarTypes [(v, ScalarT tau)] $
                     k (is:.E (VarE v))
        return $ ForE ParFor [v] [unE n] p
      where
        tau :: ScalarType
        tau = typeOf (undefined :: Exp t Ix)

    parfor (sh:.(n :: Exp t Ix)) = do
        is <- parfor sh
        v  <- gensym "i"
        shift $ \k -> do
        p <- reset $ extendVarTypes [(v, ScalarT tau)] $
                         k (is:.E (VarE v))
        case p of
          ForE ParFor vs is' e -> return $ ForE ParFor (vs ++ [v]) (is' ++ [unE n]) e
          e                    -> return $ ForE ParFor [v] [unE n] e
      where
        tau :: ScalarType
        tau = typeOf (undefined :: Exp t Ix)

-- Common dimensions
type DIM0 t = Z
type DIM1 t = DIM0 t :. Exp t Ix
type DIM2 t = DIM1 t :. Exp t Ix

ix1 :: Exp t Ix -> DIM1 t
{-# INLINE ix1 #-}
ix1 x = Z :. x

ix2 :: Exp t Ix -> Exp t Ix -> DIM2 t
{-# INLINE ix2 #-}
ix2 y x = Z :. y :. x

-- 'Rsh' is a type function from Nikola shapes to Repa shapes.
type family Rsh a :: *
type instance Rsh Z                = R.Z
type instance Rsh (sh :. Exp t Ix) = Rsh sh R.:. Int

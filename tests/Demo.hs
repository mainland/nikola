{-# LANGUAGE TemplateHaskell #-}

module Demo where

import Prelude hiding (map, zipWith, zipWith3)

import Data.Vector.Storable (Vector)
import Nikola

f :: Exp (Vector Float) -> Exp (Vector Float)
f = map inc

--inc :: Exp Float -> Exp Float
inc = vapply $ \x -> x + 1

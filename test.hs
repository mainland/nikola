{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Prelude hiding ((++), map, replicate, reverse, zipWith)
import qualified Prelude as P

import Data.Int

import Data.Array.Nikola.Backend.CUDA
import Data.Array.Nikola.Backend.CUDA.Haskell

type F = Float

type Complex = (Exp F, Exp F)

magnitude :: Complex -> Exp F
magnitude (x,y) = x*x + y*y

instance Num Complex where
    (x,y) + (x',y') = (x+x' ,y+y')
    (x,y) - (x',y') = (x-x', y-y')
    (x,y) * (x',y') = (x*x'-y*y', x*y'+y*x')
    negate (x,y)    = (negate x, negate y)
    abs z           = (magnitude z, 0)
    signum (0,0)    = 0
    signum z@(x,y)  = (x/r, y/r) where r = magnitude z
    fromInteger n   = (fromInteger n, 0)

f :: Array M DIM1 Complex -> Array M DIM1 Complex -> Array D DIM1 (Complex, Exp Int32)
f = zipWith (\x y -> (x*y, 0))

g :: [(F, F)] -> [(F, F)] -> [((F, F), Int32)]
g = compile f

main :: IO ()
main = do
    print $ g ([1..10] `P.zip` [2..11]) ([1..10] `P.zip` [2..11])
    -- defaultMain f

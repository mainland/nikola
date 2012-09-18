{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RebindableSyntax #-}

-- |
-- Module      : BlackScholes.Nikola
-- Copyright   : (c) The President and Fellows of Harvard College 2009-2010
-- Copyright   : (c) Geoffrey Mainland 2012
-- License     : BSD-style
--
-- Maintainer  : Geoffrey Mainland <mainland@apeiron.net>
-- Stability   : experimental
-- Portability : non-portable

module BlackScholes.Nikola (
    blackscholes
  ) where

import Prelude hiding (zipWith3)

import Data.Array.Nikola.Backend.CUDA

-- Nikola's compilation mechanism doesn't play well with polymorphism, so to
-- make the @blackscholes@ function polymorphic in the numeric type, we need to
-- pass an extra unused argument that plays the role of a type lambda. Pretty
-- disgusting.
blackscholes :: (Ord a, Floating (Exp a), IsElem (Exp a))
             => a
             -> Vector M (Exp a)
             -> Vector M (Exp a)
             -> Vector M (Exp a)
             -> Exp a
             -> Exp a
             -> Vector D (Exp a)
blackscholes _ ss xs ts r v =
    zipWith3 (\s x t -> blackscholes' True s x t r v) ss xs ts

blackscholes' :: (Ord a, Floating (Exp a), IsElem (Exp a))
              => Bool  -- @True@ for call, @False@ for put
              -> Exp a -- Stock price
              -> Exp a -- Option strike
              -> Exp a -- Option years
              -> Exp a -- Riskless rate
              -> Exp a -- Volatility rate
              -> Exp a
blackscholes' isCall s x t r v | isCall    = call
                               | otherwise = put
  where
    call = s * normcdf d1 - x*exp (-r*t) * normcdf d2
    put  = x * exp (-r*t) * normcdf (-d2) - s * normcdf (-d1)
    d1   = (log(s/x) + (r+v*v/2)*t)/(v*sqrt t)
    d2   = d1 - v*sqrt t

normcdf :: (Ord a, Floating (Exp a), IsElem (Exp a))
        => Exp a -> Exp a
normcdf = vapply normcdf'
  where
    normcdf' x = if x .<. 0 then 1 - w else w
      where
        w = 1.0 - 1.0 / sqrt (2.0 * pi) * exp(-l*l / 2.0) * poly k
        k = 1.0 / (1.0 + 0.2316419 * l)
        l = abs x
        poly = horner coeff
        coeff = [0.0,0.31938153,-0.356563782,1.781477937,-1.821255978,1.330274429]

horner :: Num a => [a] -> a -> a
horner coeff x = foldr1 madd coeff
  where
    madd a b = b*x + a

-- |
-- Module      : BlackScholes.Vector
-- Copyright   : (c) The President and Fellows of Harvard College 2009-2010
-- Copyright   : (c) Geoffrey Mainland 2012
-- License     : BSD-style
--
-- Maintainer  : Geoffrey Mainland <mainland@apeiron.net>
-- Stability   : experimental
-- Portability : non-portable

module BlackScholes.Vector (
    blackscholes
  ) where

type F = Float

blackscholes :: Bool -- @True@ for call, @False@ for put
             -> F    -- Stock price
             -> F    -- Option strike
             -> F    -- Option years
             -> F    -- Riskless rate
             -> F    -- Volatility rate
             -> F
{-# INLINE blackscholes #-}
blackscholes isCall s x t r v | isCall    = call
                              | otherwise = put
  where
    call = s * normcdf d1 - x*exp (-r*t) * normcdf d2
    put  = x * exp (-r*t) * normcdf (-d2) - s * normcdf (-d1)
    d1   = (log(s/x) + (r+v*v/2)*t)/(v*sqrt t)
    d2   = d1 - v*sqrt t

normcdf :: F -> F
{-# INLINE normcdf #-}
normcdf x | x < 0     = 1 -w
          | otherwise =  w
  where
    w = 1.0 - 1.0 / sqrt (2.0 * pi) * exp(-l*l / 2.0) * poly k
    k = 1.0 / (1.0 + 0.2316419 * l)
    l = abs x
    poly = horner coeff
    coeff = [0.0,0.31938153,-0.356563782,1.781477937,-1.821255978,1.330274429]

horner :: [F] -> F -> F
{-# INLINE horner #-}
horner coeff x = foldr1 madd coeff
  where
    madd a b = b*x + a

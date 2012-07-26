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

import Prelude hiding ((++), map, replicate, reverse, zipWith3)

import Data.Array.Nikola.Backend.CUDA

main :: IO ()
main =
    defaultMain blackscholes

blackscholes :: Array M DIM1 (Exp Double)
             -> Array M DIM1 (Exp Double)
             -> Array M DIM1 (Exp Double)
             -> Exp Double
             -> Exp Double
             -> Array D DIM1 (Exp Double)
blackscholes ss xs ts r v =
    zipWith3 (\s x t -> blackscholes1 True s x t r v) ss xs ts

blackscholes1 :: Bool       -- @True@ for call, @False@ for put
              -> Exp Double -- Stock price
              -> Exp Double -- Option strike
              -> Exp Double -- Option years
              -> Exp Double -- Riskless rate
              -> Exp Double -- Volatility rate
              -> Exp Double
blackscholes1 isCall s x t r v | isCall    = call
                               | otherwise = put
  where
    call = s * vapply normcdf d1 - x*exp (-r*t) * vapply normcdf d2
    put  = x * exp (-r*t) * vapply normcdf (-d2) - s * vapply normcdf (-d1)
    d1   = (log(s/x) + (r+v*v/2)*t)/(v*sqrt t)
    d2   = d1 - v*sqrt t

normcdf :: Exp Double -> Exp Double
normcdf x = if x .<. 0 then 1 - w else w
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

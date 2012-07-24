module BlackScholes.Nikola (
    blackscholes
  ) where

import Prelude hiding (zipWith3)

import qualified Data.Vector.Storable as V

import Data.Array.Nikola

blackscholes :: Exp (V.Vector Float)
             -> Exp (V.Vector Float)
             -> Exp (V.Vector Float)
             -> Exp Float
             -> Exp Float
             -> Exp (V.Vector Float)
blackscholes ss xs ts r v =
    zipWith3 (\s x t -> blackscholes' True s x t r v) ss xs ts

blackscholes' :: Bool      -- @True@ for call, @False@ for put
              -> Exp Float -- Stock price
              -> Exp Float -- Option strike
              -> Exp Float -- Option years
              -> Exp Float -- Riskless rate
              -> Exp Float -- Volatility rate
              -> Exp Float
blackscholes' isCall s x t r v | isCall    = call
                               | otherwise = put
  where
    call = s * normcdf d1 - x*exp (-r*t) * normcdf d2
    put  = x * exp (-r*t) * normcdf (-d2) - s * normcdf (-d1)
    d1   = (log(s/x) + (r+v*v/2)*t)/(v*sqrt t)
    d2   = d1 - v*sqrt t

normcdf :: Exp Float -> Exp Float
normcdf = vapply normcdf'
  where
    normcdf' x = (x .<. 0) ? (1 - w, w)
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

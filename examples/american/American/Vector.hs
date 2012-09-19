module American.Vector (binom) where

import Data.List (foldl')
import qualified Data.Vector.Storable as V

--
-- This code is taken from Ken Friis Larsen's implementation of pricing for
-- American options available at <http://github.com/kfl/american-options>.
--

type F = Double

-- Pointwise manipulation of vectors and scalars
v1 ^*^ v2 = V.zipWith (*) v1 v2
v1 ^+^ v2 = V.zipWith (+) v1 v2
c -^ v = V.map (c -) v
c *^ v = V.map (c *) v

pmax :: V.Vector F -> F -> V.Vector F
pmax v c = V.map (max c) v

ppmax :: V.Vector F -> V.Vector F -> V.Vector F
ppmax = V.zipWith max

binom :: Int -> F
binom expiry = V.head first
  where
    uPow :: V.Vector F
    uPow = V.generate (n+1) (u^)

    dPow :: V.Vector F
    dPow = V.reverse $ V.generate (n+1) (d^)

    st :: V.Vector F
    st = s0 *^ (uPow ^*^ dPow)

    finalPut :: V.Vector F
    finalPut = pmax (strike -^ st) 0

-- for (i in n:1) {
--   St<-S0*u.pow[1:i]*d.pow[i:1]
--   put[1:i]<-pmax(strike-St,(qUR*put[2:(i+1)]+qDR*put[1:i]))
-- }
    first :: V.Vector F
    first = foldl' prevPut finalPut [n, n-1 .. 1]

    prevPut :: V.Vector F -> Int -> V.Vector F
    prevPut put i = ppmax(strike -^ st) ((qUR *^ V.tail put) ^+^ (qDR *^ V.init put))
      where st = s0 *^ ((V.take i uPow) ^*^ (V.drop (n+1-i) dPow))

    -- standard econ parameters
    strike = 100
    bankDays = 252
    s0 = 100
    r = 0.03; alpha = 0.07; sigma = 0.20

    n = expiry*bankDays
    dt = fromIntegral expiry/fromIntegral n
    u = exp(alpha*dt+sigma*sqrt dt)
    d = exp(alpha*dt-sigma*sqrt dt)
    stepR = exp(r*dt)
    q = (stepR-d)/(u-d)
    qUR = q/stepR; qDR = (1-q)/stepR

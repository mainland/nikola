-- |
-- Module      : Data.Array.Nikola.Util.Statistics
-- Copyright   : (c) Geoffrey Mainland 2012
-- License     : BSD-style
--
-- Maintainer  : Geoffrey Mainland <mainland@apeiron.net>
-- Stability   : experimental
-- Portability : non-portable

module Data.Array.Nikola.Util.Statistics (
    maxDelta,
    l1Norm,
    validateL1Norm
  ) where

import Data.Vector.Generic as V
import Text.Printf

deltas :: (Vector v a, Num a)
       => v a -> v a -> v a
deltas v1 v2 = V.zipWith (\x y -> abs (x-y)) v1 v2

maxDelta :: (Vector v a, Ord a, Num a)
         => v a -> v a -> a
maxDelta v1 v2 = V.maximum (deltas v1 v2)

l1Norm :: (Vector v a, Fractional a)
       => v a -> v a -> a
l1Norm v1 v2 = V.sum (deltas v1 v2) / V.sum (V.map abs v2)

validateL1Norm :: (Vector v a, Ord a, PrintfArg a, Fractional a)
               => a -> String -> v a -> v a -> IO ()
validateL1Norm epsilon desc v1 v2 = do
    putStrLn desc
    putStrLn $ printf "   L1 norm = %1.4e" (l1Norm v1 v2)
    putStrLn $ printf " max delta = %1.4e" (maxDelta v1 v2)
    if (maxDelta v1 v2 > epsilon)
       then fail $ printf "FAILURE: max delta greater than %1.4e" epsilon
       else putStrLn "SUCCESS"

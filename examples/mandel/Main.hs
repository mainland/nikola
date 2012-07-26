module Main where

import Data.Array.Nikola.Backend.CUDA

import Mandel

{-
mandelbrot :: F -> F -> F -> F -> Int -> Int -> Int
           -> Array D DIM2 (Complex, Exp Int32)
mandelbrot x y x' y' screenX screenY depth =
    iterate (step cs) zs0 !! depth
  where
    cs  = genPlane x y x' y' screenX screenY
    zs0 = mkinit cs
-}

main :: IO ()
main = do
    defaultMain genPlane
    defaultMain mkinit
    defaultMain step

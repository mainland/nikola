module Mandelbrot.Types where

import Data.Array.Repa
import Data.Int
import Data.Word

type I = Int32

type R = Double

type RGBA = Word32

type Bitmap r = Array r DIM2 RGBA

type Complex = (R, R)

type ComplexPlane r = Array r DIM2 Complex

type StepPlane r = Array r DIM2 (Complex, I)

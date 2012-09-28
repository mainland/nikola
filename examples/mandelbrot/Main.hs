module Main where

import Data.Array.Repa
import Data.Array.Repa.Repr.ForeignPtr
import Data.Word
import Foreign (castForeignPtr)
import qualified Graphics.Gloss as G
import System.IO.Unsafe (unsafePerformIO)

--import qualified Mandelbrot.Nikola.Implementation as MN
import qualified Mandelbrot.Repa as MR

type RGBA = Word32

makePicture :: Array F DIM2 RGBA -> G.Picture
makePicture arr = G.bitmapOfForeignPtr h w (castForeignPtr (toForeignPtr arr)) False
  where
    h, w :: Int
    Z:.h:.w = extent arr

main :: IO ()
main = do
    let size  = 512
        limit = 255
        lowx  = -0.25
        lowy  = -1.0
        highx =  0.0
        highy = -0.75
    let image = MR.mandelbrot lowx lowy highx highy size size limit >>= MR.prettyMandelbrot limit
    G.display (G.InWindow "Mandelbrot" (size, size) (10, 10)) G.black
         (makePicture (unsafePerformIO image))

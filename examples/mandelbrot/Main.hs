module Main where

import Data.Array.Repa
import Data.Array.Repa.Repr.ForeignPtr
import Data.Word
import Foreign (castForeignPtr)
import qualified Graphics.Gloss as G
import System.Environment (getArgs)
import System.IO.Unsafe (unsafePerformIO)

import Config
import ParseConfig
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
    (opts, _) <- getArgs >>= parseArgs defaultConfig
    let size  = fromLJust confSize opts
        limit = fromLJust confLimit opts
        lowx  = -0.25
        lowy  = -1.0
        highx =  0.0
        highy = -0.75
    let image = MR.mandelbrot lowx lowy highx highy size size limit >>= MR.prettyMandelbrot limit
    G.display (G.InWindow "Mandelbrot" (size, size) (10, 10)) G.black
         (makePicture (unsafePerformIO image))

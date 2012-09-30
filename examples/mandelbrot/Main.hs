module Main where

import Data.Array.Repa
import Data.Array.Repa.Repr.ForeignPtr
import Foreign (castForeignPtr)
import qualified Graphics.Gloss as G
import System.Environment (getArgs)
import System.IO.Unsafe (unsafePerformIO)

import Config
import ParseConfig

import qualified Mandelbrot.Nikola as MN
import qualified Mandelbrot.Repa as MR
import Mandelbrot.Types

makePicture :: Bitmap F -> G.Picture
makePicture arr =
    G.bitmapOfForeignPtr h w (castForeignPtr (toForeignPtr arr)) False
  where
    h, w :: Int
    Z:.h:.w = extent arr

main :: IO ()
main = do
    (opts, _) <- getArgs >>= parseArgs defaultConfig
    let size  = fromIntegral $ fromLJust confSize opts
        limit = fromIntegral $ fromLJust confLimit opts
        lowx  = -0.25
        lowy  = -1.0
        highx =  0.0
        highy = -0.75
    let image = if fromLJust confNikola opts
                then MN.prettyMandelbrot limit $
                     MN.mandelbrot lowx lowy highx highy size size limit
                else unsafePerformIO $
                     MR.mandelbrot lowx lowy highx highy size size limit >>=
                     MR.prettyMandelbrot limit
    G.display (G.InWindow "Mandelbrot" (fromIntegral size, fromIntegral size) (10, 10)) G.black
         (makePicture image)

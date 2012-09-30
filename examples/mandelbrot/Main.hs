module Main where

import Data.Array.Repa
import Data.Array.Repa.Repr.ForeignPtr
import Data.Int
import Foreign (castForeignPtr)
import System.Environment (getArgs)
import System.IO.Unsafe (unsafePerformIO)

import Config
import ParseConfig

import qualified Mandelbrot.Nikola as MN
import qualified Mandelbrot.Repa as MR
import Mandelbrot.Types

--import qualified Graphics.Gloss as G
import qualified GUI as G

main :: IO ()
main = do
    (opts, _) <- getArgs >>= parseArgs defaultConfig
    let size  = fromIntegral $ fromLJust confSize opts
        limit = fromIntegral $ fromLJust confLimit opts
        lowx  = -0.25
        lowy  = -1.0
        highx =  0.0
        highy = -0.75
    let image = case  fromLJust confBackend opts of
                  Nikola ->
                      MN.prettyMandelbrot limit $
                      MN.mandelbrot lowx lowy highx highy size size limit
                  Repa ->
                      unsafePerformIO $
                      MR.mandelbrot lowx lowy highx highy size size limit >>=
                      MR.prettyMandelbrot limit
    display size image

display :: Int32 -> Bitmap F -> IO ()
display size arr = do
    G.display (G.InWindow "Mandelbrot" (fromIntegral size, fromIntegral size) (10, 10))
              pic
  where
    pic :: G.Picture
    pic = G.bitmapOfForeignPtr h w (castForeignPtr (toForeignPtr arr))

    h, w :: Int
    Z:.h:.w = extent arr

{-
display :: Int32 -> Bitmap F -> IO ()
display size arr = do
    G.display (G.InWindow "Mandelbrot" (fromIntegral size, fromIntegral size) (10, 10))
              G.black
              pic
  where
    pic :: G.Picture
    pic = G.bitmapOfForeignPtr h w (castForeignPtr (toForeignPtr arr)) False

    h, w :: Int
    Z:.h:.w = extent arr
-}

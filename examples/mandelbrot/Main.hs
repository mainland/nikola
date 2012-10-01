module Main where

import Data.Array.Repa
import Data.Array.Repa.Repr.ForeignPtr
import Data.Int
import Foreign (castForeignPtr)
import System.Environment (getArgs)

import Config
import ParseConfig

import qualified Mandelbrot.NikolaV1 as MN1
import qualified Mandelbrot.RepaV1 as MR1
import qualified Mandelbrot.RepaV2 as MR2
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
    image <- case fromLJust confBackend opts of
               RepaV1 ->
                   MR1.mandelbrot lowx lowy highx highy size size limit >>=
                   MR1.prettyMandelbrot limit
               RepaV2 ->
                   MR2.mandelbrot lowx lowy highx highy size size limit >>=
                   MR2.prettyMandelbrot limit
               Repa ->
                   MR2.mandelbrot lowx lowy highx highy size size limit >>=
                   MR2.prettyMandelbrot limit
               NikolaV1 ->
                   MN1.mandelbrot lowx lowy highx highy size size limit >>=
                   MN1.prettyMandelbrot limit
               Nikola ->
                   MN1.mandelbrot lowx lowy highx highy size size limit >>=
                   MN1.prettyMandelbrot limit
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

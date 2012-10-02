module Main where

import Control.Applicative
import Data.Array.Repa
import Data.Array.Repa.Repr.ForeignPtr
import Foreign (castForeignPtr)
import System.Environment (getArgs)

import Config
import ParseConfig

import qualified Mandelbrot.NikolaV1 as MN1
import qualified Mandelbrot.NikolaV2 as MN2
import qualified Mandelbrot.RepaV1 as MR1
import qualified Mandelbrot.RepaV2 as MR2
import Mandelbrot.Types

import qualified GUI as G

defaultView :: G.View
defaultView = G.View { G.left  = -0.25
                     , G.bot   = -1.0
                     , G.right =  0.0
                     , G.top   = -0.75
                     }

main :: IO ()
main = do
    (opts, _) <- getArgs >>= parseArgs defaultConfig
    let size  = fromIntegral $ fromLJust confSize opts
        limit = fromIntegral $ fromLJust confLimit opts
        f     = frameGen (fromLJust confBackend opts) limit
    let disp = G.InWindow "Mandelbrot" (fromIntegral size, fromIntegral size) (10, 10)
    G.display disp defaultView f

frameGen :: Backend -> I -> Float -> G.View -> (Int, Int) -> IO G.Picture
frameGen backend limit _ (G.View lowx lowy highx highy) (sizeX, sizeY) = do
    bitmapToPicture <$> go backend (fromIntegral sizeX, fromIntegral sizeY)
  where
    lowx', lowy', highx', highy' :: R
    lowx'  = realToFrac lowx
    lowy'  = realToFrac lowy
    highx' = realToFrac highx
    highy' = realToFrac highy

    go :: Backend -> (I, I) -> IO (Bitmap F)
    go be (sizeX, sizeY)
      | be == NikolaV1
      = MN1.mandelbrot lowx' lowy' highx' highy' sizeX sizeY limit >>=
        MN1.prettyMandelbrot limit

      | be == NikolaV2 || be == Nikola
      = MN2.mandelbrot lowx' lowy' highx' highy' sizeX sizeY limit >>=
        MN2.prettyMandelbrot limit

      | be == RepaV1
      = MR1.mandelbrot lowx' lowy' highx' highy' sizeX sizeY limit >>=
        MR1.prettyMandelbrot limit

      | otherwise
      = MR2.mandelbrot lowx' lowy' highx' highy' sizeX sizeY limit >>=
        MR2.prettyMandelbrot limit

    bitmapToPicture :: Bitmap F -> G.Picture
    bitmapToPicture arr = G.bitmapOfForeignPtr h w (castForeignPtr (toForeignPtr arr))
      where
        h, w :: Int
        Z:.h:.w = extent arr

module Main where

import Data.Array.Repa
import Data.Array.Repa.Eval
import Data.Array.Repa.Repr.ForeignPtr
import Data.Word
import Foreign (castForeignPtr)
import System.Environment (getArgs)

import qualified Mandelbrot.NikolaV1 as MN1
import qualified Mandelbrot.NikolaV2 as MN2
import qualified Mandelbrot.NikolaV3 as MN3
import qualified Mandelbrot.RepaV1 as MR1
import qualified Mandelbrot.RepaV2 as MR2

import Config
import ParseConfig
import qualified GUI as G

type R = Double

type RGBA = Word32

type Bitmap r = Array r DIM2 RGBA

type MBitmap r = MVec r RGBA

defaultView :: G.View
defaultView = G.View { G.left  = -0.25
                     , G.bot   = -1.0
                     , G.right =  0.0
                     , G.top   = -0.75
                     }

main :: IO ()
main = do
    (opts, _) <- getArgs >>= parseArgs defaultConfig
    let size    = fromIntegral $ fromLJust confSize opts
        limit   = fromIntegral $ fromLJust confLimit opts
        backend = fromLJust confBackend opts
        disp    = G.InWindow "Mandelbrot" (size, size) (10, 10)
    G.display disp defaultView (frameGenerator backend limit)

frameGenerator :: Backend -> Int -> IO G.FrameGen
frameGenerator RepaV1 limit = return f
  where
    f :: G.FrameGen
    f _ (G.View lowx lowy highx highy) (sizeX, sizeY) = do
        bmap <- MR1.mandelbrotImage lowx lowy highx highy sizeX sizeY limit
        return $ bitmapToPicture bmap

frameGenerator RepaV2 limit = do
    gen <- MR2.mandelbrotImageGenerator
    return $ f gen
  where
    f gen _ (G.View lowx lowy highx highy) (sizeX, sizeY) = do
        bmap <- gen lowx lowy highx highy sizeX sizeY limit
        return $ bitmapToPicture bmap

frameGenerator Repa limit = frameGenerator RepaV2 limit

frameGenerator NikolaV1 limit = return f
  where
    f :: G.FrameGen
    f _ (G.View lowx lowy highx highy) (sizeX, sizeY) = do
        bmap <- MN1.mandelbrotImage lowx lowy highx highy sizeX sizeY limit
        return $ bitmapToPicture bmap

frameGenerator NikolaV2 limit = do
    gen <- MN2.mandelbrotImageGenerator
    return $ f gen
  where
    f gen _ (G.View lowx lowy highx highy) (sizeX, sizeY) = do
        bmap <- gen lowx lowy highx highy sizeX sizeY limit
        return $ bitmapToPicture bmap

frameGenerator NikolaV3 limit = do
    gen <- MN3.mandelbrotImageGenerator
    return $ f gen
  where
    f gen _ (G.View lowx lowy highx highy) (sizeX, sizeY) = do
        bmap <- gen lowx lowy highx highy sizeX sizeY limit
        return $ bitmapToPicture bmap

frameGenerator Nikola limit = frameGenerator NikolaV3 limit

bitmapToPicture :: Bitmap F -> G.Picture
bitmapToPicture arr =
    G.bitmapOfForeignPtr h w (castForeignPtr (toForeignPtr arr))
  where
    h, w :: Int
    Z:.h:.w = extent arr

{-
    bitmapToPicture :: Bitmap F -> IO G.Picture
    bitmapToPicture arr = do
        pic@(G.PBO _ _ pbo) <- G.pboOfForeignPtr h w (castForeignPtr (toForeignPtr arr))
        ref <- CUGL.registerBuffer pbo CUG.RegisterNone
        CUG.mapResources [ref] Nothing
        CUG.getMappedPointer ref >>= print
        CUG.unmapResources [ref] Nothing
        return pic
      where
        h, w :: Int
        Z:.h:.w = extent arr
-}

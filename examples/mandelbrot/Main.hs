module Main where

import Control.DeepSeq
import qualified Criterion.Main as C
import Data.Array.Repa hiding ((++))
import Data.Array.Repa.Eval
import Data.Array.Repa.Repr.ForeignPtr
import Data.Word
import Foreign (castForeignPtr)
import qualified Graphics.Rendering.OpenGL as GL
import System.Environment (getArgs, withArgs)

import qualified Data.Array.Nikola.Backend.CUDA as N

import qualified Mandelbrot.NikolaV1 as MN1
import qualified Mandelbrot.NikolaV2 as MN2
import qualified Mandelbrot.NikolaV3 as MN3
import qualified Mandelbrot.NikolaV4 as MN4
import qualified Mandelbrot.RepaV1 as MR1
import qualified Mandelbrot.RepaV2 as MR2
import qualified Mandelbrot.RepaV3 as MR3

import Config
import ParseConfig
import qualified GUI as G

type R = Double

type RGBA = Word32

type Bitmap r = Array r DIM2 RGBA

type MBitmap r = MVec r RGBA

instance (Shape sh, Source r e) => NFData (Array r sh e) where
    rnf arr = deepSeqArray arr ()

instance NFData GL.BufferObject where
    rnf pbo = seq pbo ()

defaultView :: G.View
defaultView = G.View { G.left  = -0.25
                     , G.bot   = -1.0
                     , G.right =  0.0
                     , G.top   = -0.75
                     }

main :: IO ()
main = do
    args <- getArgs
    case args of
      "--benchmark":args' -> doBenchmark args'
      _                   -> doMandelbrot

doBenchmark :: [String] -> IO ()
doBenchmark args =
    withArgs args $ do
    G.initializeGLUT False
    G.openWindowGLUT disp
    N.initializeCUDAGLCtx N.DeviceListAll
    repav2Gen   <- MR2.mandelbrotImageGenerator
    repav3Gen   <- MR3.mandelbrotImageGenerator
    nikolav2Gen <- MN2.mandelbrotImageGenerator
    nikolav3Gen <- MN3.mandelbrotImageGenerator
    nikolav4Gen <- MN4.mandelbrotImageGenerator
    let generateBitmapFrame :: Backend -> G.View -> Int -> Int -> IO (Bitmap F)
        generateBitmapFrame RepaV1 (G.View lowx lowy highx highy) size limit =
            MR1.mandelbrotImage lowx lowy highx highy size size limit

        generateBitmapFrame RepaV2 (G.View lowx lowy highx highy) size limit =
            repav2Gen lowx lowy highx highy size size limit

        generateBitmapFrame RepaV3 (G.View lowx lowy highx highy) size limit =
            repav3Gen lowx lowy highx highy size size limit

        generateBitmapFrame NikolaV1 (G.View lowx lowy highx highy) size limit =
            MN1.mandelbrotImage lowx lowy highx highy size size limit

        generateBitmapFrame NikolaV2 (G.View lowx lowy highx highy) size limit =
            nikolav2Gen lowx lowy highx highy size size limit

        generateBitmapFrame NikolaV3 (G.View lowx lowy highx highy) size limit =
            nikolav3Gen lowx lowy highx highy size size limit

        generateBitmapFrame backend _ _ _ =
            fail $ "Cannot generate bitmap for" ++ show backend

    let generatePBOFrame :: Backend -> G.View -> Int -> Int -> IO GL.BufferObject
        generatePBOFrame NikolaV4 (G.View lowx lowy highx highy) size limit =
            nikolav4Gen lowx lowy highx highy size size limit

        generatePBOFrame backend _ _ _ =
            fail $ "Cannot generate PBO for" ++ show backend

    C.defaultMain
         [C.bench "Repa V1" $ C.nfIO (generateBitmapFrame RepaV1 defaultView size limit)
         ,C.bench "Repa V2" $ C.nfIO (generateBitmapFrame RepaV2 defaultView size limit)
         ,C.bench "Repa V3" $ C.nfIO (generateBitmapFrame RepaV3 defaultView size limit)
         ,C.bench "Nikola V1" $ C.nfIO (generateBitmapFrame NikolaV1 defaultView size limit)
         ,C.bench "Nikola V2" $ C.nfIO (generateBitmapFrame NikolaV2 defaultView size limit)
         ,C.bench "Nikola V3" $ C.nfIO (generateBitmapFrame NikolaV3 defaultView size limit)
         ,C.bench "Nikola V4" $ C.nfIO (generatePBOFrame NikolaV4 defaultView size limit)
         ]
  where
    size  = 512
    limit = 255
    disp  = G.InWindow "Mandelbrot" (size, size) (10, 10)

doMandelbrot :: IO ()
doMandelbrot = do
    (config, _) <- getArgs >>= parseArgs defaultConfig
    let size    = fromIntegral $ fromLJust confSize config
        limit   = fromIntegral $ fromLJust confLimit config
        backend = fromLJust confBackend config
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

frameGenerator RepaV3 limit = do
    gen <- MR3.mandelbrotImageGenerator
    return $ f gen
  where
    f gen _ (G.View lowx lowy highx highy) (sizeX, sizeY) = do
        bmap <- gen lowx lowy highx highy sizeX sizeY limit
        return $ bitmapToPicture bmap

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

frameGenerator NikolaV4 limit = do
    gen <- MN4.mandelbrotImageGenerator
    return $ f gen
  where
    f gen _ (G.View lowx lowy highx highy) (sizeX, sizeY) = do
        pbo <- gen lowx lowy highx highy sizeX sizeY limit
        return $ G.PBO sizeX sizeY pbo

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

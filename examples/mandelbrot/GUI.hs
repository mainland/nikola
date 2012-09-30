{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}

--
-- This code is adapted from Ben Lippmeier's Gloss library
--

module GUI where

import Data.IORef
import Data.List (find)
import Data.Word
import Control.Applicative
import Control.Monad
import Data.Maybe (isJust)
--import Control.Concurrent
import Foreign (ForeignPtr,
                withForeignPtr)
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLUT (get,($=))
import qualified Graphics.UI.GLUT as GLUT
--import qualified System.Exit as System
import System.Mem.StableName
import Unsafe.Coerce

import GUI.Commands

data Display = InWindow String       -- ^ Window name
                        (Int, Int)   -- ^ Size
                        (Int, Int)   -- ^ Position
             | FullScreen (Int, Int) -- ^ Size

data Picture = Bitmap Int        -- ^ Width
                      Int        -- ^ Height
                      BitmapData -- ^ Bitmap data

data BitmapData = BitmapData !Int                -- ^ Length (in bytes)
                             !(ForeignPtr Word8) -- ^ Pointer to data

data State = State
    { stateColor       :: Bool
    , stateWireframe   :: Bool
    , stateBlendAlpha  :: Bool
    , stateLineSmooth  :: Bool
    , stateViewPort    :: IORef ViewPort
    , stateViewControl :: IORef ViewControl
    , statePicture     :: IORef Picture
    , stateTextures    :: IORef [Texture]
    }

data ViewPort = ViewPort
    { viewPortTranslate :: (Float, Float)
    , viewPortRotate    :: Float
    , viewPortScale     :: Float
    }
  deriving (Eq, Show)

data ViewControl = ViewControl
    { viewControlScaleStep     :: Float
    , viewControlRotateFactor  :: Float
    , viewControlTranslateMark :: Maybe (GL.GLint, GL.GLint)
    , viewControlRotateMark    :: Maybe (GL.GLint, GL.GLint)
    }
  deriving (Eq, Show)

data Texture = Texture
    { texName    :: StableName BitmapData
    , texWidth   :: Int
    , texHeight  :: Int
    , texData    :: ForeignPtr Word8
    , texObject  :: GL.TextureObject
    , texCacheMe :: Bool
    }

defaultViewPort :: ViewPort
defaultViewPort = ViewPort
    { viewPortTranslate = (0, 0)
    , viewPortRotate    = 0
    , viewPortScale     = 1
    }

defaultViewControl :: ViewControl
defaultViewControl = ViewControl
    { viewControlScaleStep     = 0.85
    , viewControlRotateFactor  = 0.6
    , viewControlTranslateMark = Nothing
    , viewControlRotateMark    = Nothing
    }

defaultState :: Picture -> IO State
defaultState pic = do
    viewPortRef    <- newIORef defaultViewPort
    viewControlRef <- newIORef defaultViewControl
    picRef         <- newIORef pic
    texturesRef    <- newIORef []
    return State { stateColor       = True
                 , stateWireframe   = False
                 , stateBlendAlpha  = True
                 , stateLineSmooth  = False
                 , stateViewPort    = viewPortRef
                 , stateViewControl = viewControlRef
                 , statePicture     = picRef
                 , stateTextures    = texturesRef
                 }

bitmapOfForeignPtr :: Int -> Int -> ForeignPtr Word8 -> Picture
bitmapOfForeignPtr width height fptr =
    Bitmap width height bdata
  where
    len   = width * height * 4
    bdata = BitmapData len fptr

display :: Display  -- ^ Display mode.
        -> Picture  -- ^ The picture to draw.
        -> IO ()
display disp pic = do
    initializeGLUT False
    openWindowGLUT disp
    state    <- defaultState pic
    stateRef <- newIORef state
    GLUT.displayCallback       $= callbackDisplay stateRef
    GLUT.reshapeCallback       $= Just (callbackReshape stateRef)
    GLUT.keyboardMouseCallback $= Just (callbackKeyMouse stateRef)
    GLUT.motionCallback        $= Just (callbackMotion stateRef)
    GLUT.passiveMotionCallback $= Just (callbackMotion stateRef)
    GLUT.mainLoop

initializeGLUT :: Bool -> IO ()
initializeGLUT debug = do
    (_progName, _args)  <- GLUT.getArgsAndInitialize
    glutVersion         <- get GLUT.glutVersion
    when debug $
        putStr $ "  glutVersion        = " ++ show glutVersion   ++ "\n"

    GLUT.initialDisplayMode $= [ GLUT.RGBMode
                               , GLUT.DoubleBuffered]

    -- See if our requested display mode is possible
    displayMode         <- get GLUT.initialDisplayMode
    displayModePossible <- get GLUT.displayModePossible
    when debug
     $ do putStr $  "  displayMode        = " ++ show displayMode ++ "\n"
                 ++ "       possible      = " ++ show displayModePossible ++ "\n"
                 ++ "\n"

openWindowGLUT :: Display -> IO ()
openWindowGLUT disp = do
    open disp
    GLUT.perWindowKeyRepeat $= GLUT.PerWindowKeyRepeatOff
  where
    open :: Display -> IO ()
    open (InWindow windowName (sizeX, sizeY) (posX, posY)) = do
        GLUT.initialWindowSize $= GL.Size  (fromIntegral sizeX)
                                           (fromIntegral sizeY)
        GLUT.initialWindowPosition $= GL.Position (fromIntegral posX)
                                                  (fromIntegral posY)
        void $ GLUT.createWindow windowName

        GLUT.windowSize $= GL.Size (fromIntegral sizeX)
                                   (fromIntegral sizeY)

    open (FullScreen (sizeX, sizeY)) = do
        GLUT.gameModeCapabilities $=
                [ GLUT.Where' GLUT.GameModeWidth GLUT.IsEqualTo sizeX
                , GLUT.Where' GLUT.GameModeHeight GLUT.IsEqualTo sizeY ]
        void $ GLUT.enterGameMode

exitGLUT :: IO ()
-- exitGLUT = System.exitWith System.ExitSuccess
exitGLUT = GLUT.leaveMainLoop

callbackDisplay :: IORef State -> IO ()
callbackDisplay state = do
    GL.clear [GL.ColorBuffer, GL.DepthBuffer]
    GL.color $ GL.Color4 0 0 0 (1 :: GL.GLfloat)

    draw state

    GLUT.swapBuffers

    return ()

callbackReshape :: IORef State -> GLUT.Size -> IO ()
callbackReshape _ (GLUT.Size width height) = do
    GL.viewport $= (GL.Position 0 0, GL.Size width height)
    postRedisplay

postRedisplay :: IO ()
postRedisplay = GLUT.postRedisplay Nothing

callbackKeyMouse :: IORef State
                 -> GLUT.Key
                 -> GLUT.KeyState
                 -> GLUT.Modifiers
                 -> GLUT.Position
                 -> IO ()
callbackKeyMouse stateRef glutKey glutKeyState glutKeyMods (GLUT.Position posX posY) = do
    state                <- readIORef stateRef
    let viewPortRef      =  stateViewPort state
    let viewControlRef   =  stateViewControl state
    currentlyTranslating <- (isJust . viewControlTranslateMark) <$> readIORef viewControlRef
    currentlyRotating    <- (isJust . viewControlRotateMark)    <$> readIORef viewControlRef
    go viewPortRef viewControlRef currentlyTranslating currentlyRotating
  where
    key :: Key
    key  = glutKeyToKey glutKey

    keyState :: KeyState
    keyState = glutKeyStateToKeyState glutKeyState

    keyMods :: Modifiers
    keyMods = glutModifiersToModifiers glutKeyMods

    pos = (posX, posY)

    go :: IORef ViewPort
       -> IORef ViewControl
       -> Bool
       -> Bool
       -> IO ()
    go viewPortRef viewControlRef currentlyTranslating currentlyRotating
        -- restore viewport
        | isCommand commands CRestore key keyMods
        , keyState == Down
        = do restoreView
             postRedisplay

        -- zoom in
        | isCommand commands CBumpZoomIn key keyMods
        , keyState == Down
        = do zoomIn
             postRedisplay

        -- zoom out
        | isCommand commands CBumpZoomOut key keyMods
        , keyState == Down
        = do zoomOut
             postRedisplay

        -- bump left
        | isCommand commands CBumpLeft key keyMods
        , keyState == Down
        = do translateBy viewPortRef viewControlRef (20, 0)
             postRedisplay

        -- bump right
        | isCommand commands CBumpRight key keyMods
        , keyState == Down
        = do translateBy viewPortRef viewControlRef (-20, 0)
             postRedisplay

        -- bump up
        | isCommand commands CBumpUp key keyMods
        , keyState == Down
        = do translateBy viewPortRef viewControlRef (0, 20)
             postRedisplay

        -- bump down
        | isCommand commands CBumpDown key keyMods
        , keyState == Down
        = do translateBy viewPortRef viewControlRef (0, -20)
             postRedisplay

        -- bump clockwise
        | isCommand commands CBumpClockwise key keyMods
        , keyState == Down
        = do rotateBy viewPortRef viewControlRef (+ 5)
             postRedisplay

        -- bump counter-clockwise
        | isCommand commands CBumpCClockwise key keyMods
        , keyState == Down
        = do rotateBy viewPortRef viewControlRef (subtract 5)
             postRedisplay

        -- start translation
        | isCommand commands CTranslate key keyMods
        , keyState == Down
        , not currentlyRotating
        = do modifyIORef viewControlRef $ \s -> s { viewControlTranslateMark = Just pos }
             postRedisplay

        -- end translation
        | currentlyTranslating
        , keyState == Up
        = do modifyIORef viewControlRef $ \s -> s { viewControlTranslateMark = Nothing }
             postRedisplay

        -- start rotation
        | isCommand commands CRotate key keyMods
        , keyState == Down
        , not currentlyTranslating
        = do modifyIORef viewControlRef $ \s -> s { viewControlRotateMark = Just pos }
             postRedisplay

        -- end rotation
        | currentlyRotating
        , keyState == Up
        = do modifyIORef viewControlRef $ \s -> s { viewControlRotateMark = Nothing }
             postRedisplay

        -- default
        | otherwise = return ()
      where
        restoreView :: IO ()
        restoreView =
            modifyIORef viewPortRef $ \_ -> defaultViewPort

        zoomIn :: IO ()
        zoomIn = do
            scaleStep <- viewControlScaleStep <$> readIORef viewControlRef
            modifyIORef viewPortRef $ \s -> s { viewPortScale = viewPortScale s * scaleStep }

        zoomOut :: IO ()
        zoomOut = do
            scaleStep <- viewControlScaleStep <$> readIORef viewControlRef
            modifyIORef viewPortRef $ \s -> s { viewPortScale = viewPortScale s / scaleStep }

callbackMotion :: IORef State
               -> GLUT.Position
               -> IO ()
callbackMotion stateRef (GLUT.Position posX posY) = do
    state              <- readIORef stateRef
    let viewPortRef    =  stateViewPort state
    let viewControlRef =  stateViewControl state
    translateMark      <- viewControlTranslateMark <$> readIORef viewControlRef
    rotateMark         <- viewControlRotateMark    <$> readIORef viewControlRef
    case translateMark of
      Nothing   -> return ()
      Just mark -> motionTranslate viewPortRef viewControlRef mark (posX, posY)
    case rotateMark of
      Nothing   -> return ()
      Just mark -> motionRotate viewPortRef viewControlRef mark (posX, posY)

motionTranslate :: IORef ViewPort
                -> IORef ViewControl
                -> (GL.GLint, GL.GLint)
                -> (GL.GLint, GL.GLint)
                -> IO ()
motionTranslate viewPortRef viewControlRef (markX, markY) pos@(posX, posY) = do
    let dX = fromIntegral $ markX - posX
    let dY = fromIntegral $ markY - posY
    translateBy viewPortRef viewControlRef (dX, dY)

    modifyIORef viewControlRef $ \s ->
        s { viewControlTranslateMark = Just pos }

    postRedisplay

motionRotate :: IORef ViewPort
             -> IORef ViewControl
             -> (GL.GLint, GL.GLint)
             -> (GL.GLint, GL.GLint)
             -> IO ()
motionRotate viewPortRef viewControlRef (markX, _) pos@(posX, _) = do
    rotate       <- viewPortRotate          <$> readIORef viewPortRef
    rotateFactor <- viewControlRotateFactor <$> readIORef viewControlRef

    rotateBy viewPortRef viewControlRef (const (rotate + rotateFactor * fromIntegral (fromIntegral posX - markX)))

    modifyIORef viewControlRef $ \s ->
        s { viewControlRotateMark = Just pos }

    postRedisplay

translateBy :: IORef ViewPort
            -> IORef ViewControl
            -> (Float, Float)
            -> IO ()
translateBy viewPortRef _ (dX, dY) = do
    viewPort             <- readIORef viewPortRef
    let (transX, transY) =  viewPortTranslate viewPort
        scale            =  viewPortScale     viewPort
        r                =  viewPortRotate    viewPort

    let offset           =  (dX / scale, dY / scale)

    let (oX, oY)         =  rotateV (degToRad r) offset

    modifyIORef viewPortRef $ \s ->
        s { viewPortTranslate = (transX - oX, transY + oY) }

rotateBy :: IORef ViewPort
         -> IORef ViewControl
         -> (Float -> Float)
         -> IO ()
rotateBy viewPortRef _ delta =
    modifyIORef viewPortRef $ \s ->
        s { viewPortRotate = delta (viewPortRotate s) }

draw :: IORef State
     -> IO ()
draw stateRef = do
  state    <- readIORef stateRef
  viewPort <- readIORef (stateViewPort state)
  pic      <- readIORef (statePicture state)
  withViewPort viewPort $ do
      setLineSmooth (stateLineSmooth state)
      setBlendAlpha (stateBlendAlpha state)
      drawPicture state pic
  where
    drawPicture :: State -> Picture -> IO ()
    drawPicture state (Bitmap width height bdata) = do
        tex <- loadTexture (stateTextures state) width height bdata False

        -- Set up wrap and filtering mode
        GL.textureWrapMode GL.Texture2D GL.S $= (GL.Repeated, GL.Repeat)
        GL.textureWrapMode GL.Texture2D GL.T $= (GL.Repeated, GL.Repeat)
        GL.textureFilter   GL.Texture2D      $= ((GL.Nearest, Nothing), GL.Nearest)

        -- Enable texturing
        GL.texture GL.Texture2D $= GL.Enabled
        GL.textureFunction      $= GL.Combine

        -- Set current texture
        GL.textureBinding GL.Texture2D $= Just (texObject tex)

        -- Set to opaque
        GL.currentColor $= GL.Color4 1.0 1.0 1.0 1.0

        -- Draw textured polygon
        GL.renderPrimitive GL.Polygon
         $ zipWithM_
                (\(pX, pY) (tX, tY)
                  -> do GL.texCoord $ GL.TexCoord2 (gf tX) (gf tY)
                        GL.vertex   $ GL.Vertex2   (gf pX) (gf pY))

                (bitmapPath (fromIntegral width) (fromIntegral height))
                        [(0,0), (1.0,0), (1.0,1.0), (0,1.0)]

        -- Disable texturing
        GL.texture GL.Texture2D $= GL.Disabled

        -- Free uncachable texture objects.
        freeTexture tex

withViewPort :: ViewPort             -- ^ The viewport to use.
             -> IO ()                -- ^ The rendering action to perform.
             -> IO ()
withViewPort port action = do
    GL.matrixMode $= GL.Projection
    GL.preservingMatrix $ do
        -- setup the co-ordinate system
        GL.loadIdentity
        GL.Size sizeX sizeY <- get GLUT.windowSize
        let (sx, sy)        = (fromIntegral sizeX/2, fromIntegral sizeY/2)

        GL.ortho (-sx) sx (-sy) sy 0 (-100)

        -- draw the world
        GL.matrixMode $= GL.Modelview 0
        GL.preservingMatrix $ do
            GL.loadIdentity
            let rotate :: GL.GLfloat = realToFrac $ viewPortRotate port
            let transX :: GL.GLfloat = realToFrac $ fst $ viewPortTranslate port
            let transY :: GL.GLfloat = realToFrac $ snd $ viewPortTranslate port
            let scale  :: GL.GLfloat = realToFrac $ viewPortScale port

            -- apply the global view transforms
            GL.scale     scale  scale  1
            GL.rotate    rotate (GL.Vector3 0 0 1)
            GL.translate (GL.Vector3 transX transY 0)

            -- call the client render action
            action

        GL.matrixMode   $= GL.Projection

    GL.matrixMode   $= GL.Modelview 0

setBlendAlpha :: Bool -> IO ()
setBlendAlpha True = do
    GL.blend     $= GL.Enabled
    GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)

setBlendAlpha False = do
    GL.blend     $= GL.Disabled
    GL.blendFunc $= (GL.One, GL.Zero)

setLineSmooth :: Bool -> IO ()
setLineSmooth True  = GL.lineSmooth $= GL.Enabled
setLineSmooth False = GL.lineSmooth $= GL.Disabled

bitmapPath :: Float -> Float -> [(Float, Float)]
bitmapPath width height =
    [(-width', -height'), (width', -height'), (width', height'), (-width', height')]
 where
   width'  = width  / 2
   height' = height / 2

loadTexture  :: IORef [Texture]
             -> Int -> Int -> BitmapData
             -> Bool
             -> IO Texture
loadTexture refTextures width height bitmapData@(BitmapData _ fptr) cacheMe = do
    textures        <- readIORef refTextures
    name            <- makeStableName bitmapData
    let mTexCached  =  find (\tex -> texName   tex == name
                                  && texWidth  tex == width
                                  && texHeight tex == height)
                       textures
    case mTexCached of
      Just tex -> return tex
      Nothing  -> do tex <- installTexture
                     when cacheMe $
                         writeIORef refTextures (tex : textures)
                     return tex
  where
    installTexture :: IO Texture
    installTexture = do
        -- Allocate texture handle for texture
        [tex] <- GL.genObjectNames 1
        GL.textureBinding GL.Texture2D $= Just tex

        -- Sets the texture in bitmapData as the current texture This copies the
        -- data from the pointer into OpenGL texture memory, so it's ok if the
        -- foreignptr gets garbage collected after this.
        withForeignPtr fptr $ \ptr ->
           GL.texImage2D
                Nothing
                GL.NoProxy
                0
                GL.RGBA8
                (GL.TextureSize2D
                        (gsizei width)
                        (gsizei height))
                0
                (GL.PixelData GL.RGBA GL.UnsignedInt8888 ptr)

        -- Make a stable name that we can use to identify this data again.
        -- If the user gives us the same texture data at the same size then we
        -- can avoid loading it into texture memory again.
        name <- makeStableName bitmapData

        return Texture { texName    = name
                       , texWidth   = width
                       , texHeight  = height
                       , texData    = fptr
                       , texObject  = tex
                       , texCacheMe = cacheMe
                       }

freeTexture :: Texture -> IO ()
freeTexture tex
 | texCacheMe tex = return ()
 | otherwise      = GL.deleteObjectNames [texObject tex]

gf :: Float -> GL.GLfloat
{-# INLINE gf #-}
gf x = unsafeCoerce x

-- | Used for similar reasons to above
gsizei :: Int -> GL.GLsizei
{-# INLINE gsizei #-}
gsizei x = unsafeCoerce x

-- | Rotate a vector by an angle (in radians). +ve angle is counter-clockwise.
rotateV :: Float -> (Float, Float) -> (Float, Float)
{-# INLINE rotateV #-}
rotateV r (x, y)
 =      (  x * cos r - y * sin r
        ,  x * sin r + y * cos r)

-- | Convert degrees to radians
{-# INLINE degToRad #-}
degToRad :: Float -> Float
degToRad d      = d * pi / 180

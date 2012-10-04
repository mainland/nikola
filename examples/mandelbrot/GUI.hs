{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}

--
-- This code is adapted from Ben Lippmeier's Gloss library
--

module GUI where

import Data.IORef
import Data.Word
import Control.Applicative
import Control.Monad
import Data.Maybe (isJust)
--import Control.Concurrent
import Foreign (ForeignPtr,
                nullPtr,
                withForeignPtr)
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLUT (get,($=))
import qualified Graphics.UI.GLUT as GLUT
--import qualified System.Exit as System
import Unsafe.Coerce

import qualified Data.Array.Nikola.Backend.CUDA as N

import GUI.Commands

data Display = InWindow String       -- ^ Window name
                        (Int, Int)   -- ^ Size
                        (Int, Int)   -- ^ Position
             | FullScreen (Int, Int) -- ^ Size

displaySize :: Display -> (Int, Int)
displaySize (InWindow _ size _) = size
displaySize (FullScreen size)   = size

data Picture = Bitmap Int             -- ^ Width
                      Int             -- ^ Height
                      BitmapData      -- ^ Bitmap data
             | PBO    Int             -- ^ Width
                      Int             -- ^ Height
                      GL.BufferObject -- ^ Bitmap data

data BitmapData = BitmapData !Int                -- ^ Length (in bytes)
                             !(ForeignPtr Word8) -- ^ Pointer to data

type FrameGen = Float -> View -> (Int, Int) -> IO Picture

data State = State
    { stateColor       :: Bool
    , stateWireframe   :: Bool
    , stateBlendAlpha  :: Bool
    , stateLineSmooth  :: Bool
    , stateRealTime    :: IORef Bool
    , stateDefaultView :: View
    , stateView        :: IORef View
    , stateViewPort    :: IORef ViewPort
    , stateViewControl :: IORef ViewControl
    , stateFrameGen    :: FrameGen
    , statePicture     :: IORef Picture
    }

data View = View { left  :: Double
                 , bot   :: Double
                 , right :: Double
                 , top   :: Double
                 }
  deriving (Eq, Show)

data ViewPort = ViewPort
    { viewPortSize      :: (Int, Int)
    , viewPortTranslate :: (Float, Float)
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

data Texture = Texture { texObject :: GL.TextureObject }

defaultViewPort :: (Int, Int) -> ViewPort
defaultViewPort size = ViewPort
    { viewPortSize      = size
    , viewPortTranslate = (0, 0)
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

defaultState :: Display -> View -> FrameGen -> IO State
defaultState disp view framegen = do
    pic            <- framegen 0 view (displaySize disp)
    realTimeRef    <- newIORef True
    viewRef        <- newIORef view
    viewPortRef    <- newIORef (defaultViewPort (displaySize disp))
    viewControlRef <- newIORef defaultViewControl
    picRef         <- newIORef pic
    return State { stateColor       = True
                 , stateWireframe   = False
                 , stateBlendAlpha  = True
                 , stateLineSmooth  = False
                 , stateRealTime    = realTimeRef
                 , stateDefaultView = view
                 , stateView        = viewRef
                 , stateViewPort    = viewPortRef
                 , stateViewControl = viewControlRef
                 , stateFrameGen    = framegen
                 , statePicture     = picRef
                 }

bitmapOfForeignPtr :: Int -> Int -> ForeignPtr Word8 -> Picture
bitmapOfForeignPtr width height fptr =
    Bitmap width height bdata
  where
    len   = width * height * 4
    bdata = BitmapData len fptr

pboOfForeignPtr :: Int -> Int -> ForeignPtr Word8 -> IO Picture
pboOfForeignPtr width height fptr = do
    [pbo] <- GL.genObjectNames 1
    GL.bindBuffer GL.PixelUnpackBuffer $= Just pbo
    withForeignPtr fptr $ \ptr ->
        GL.bufferData GL.PixelUnpackBuffer $= (fromIntegral len, ptr, GL.DynamicCopy)
    GL.bindBuffer GL.PixelUnpackBuffer $= Nothing
    GLUT.reportErrors
    return $ PBO width height pbo
  where
    len = width * height * 4

display :: Display     -- ^ Display mode.
        -> View        -- ^ Default view
        -> IO FrameGen -- ^ Frame generation function
        -> IO ()
display disp view mf = do
    initializeGLUT False
    openWindowGLUT disp
    N.initializeCUDACtx
    -- This doesn't work with optimization on...it fails with the error "invalid
    -- context handle."
    --N.initializeCUDAGLCtx N.DeviceListAll
    -- CU.allocaArray 10 $ \(_ :: CU.DevicePtr Int) -> print "initializeCUDACtx allocation succeeded"
    f        <- mf
    state    <- defaultState disp view f
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
callbackReshape stateRef (GLUT.Size width height) = do
    state              <- readIORef stateRef
    realTime           <- readIORef (stateRealTime state)
    let viewRef        =  stateView state
    let viewPortRef    =  stateViewPort state
    let viewControlRef =  stateViewControl state
    resizeTo realTime viewRef viewPortRef viewControlRef (fromIntegral width, fromIntegral height)
    updatePicture stateRef

callbackKeyMouse :: IORef State
                 -> GLUT.Key
                 -> GLUT.KeyState
                 -> GLUT.Modifiers
                 -> GLUT.Position
                 -> IO ()
callbackKeyMouse stateRef glutKey glutKeyState glutKeyMods (GLUT.Position posX posY) = do
    state                <- readIORef stateRef
    realTime             <- readIORef (stateRealTime state)
    let viewRef          =  stateView state
    let viewPortRef      =  stateViewPort state
    let viewControlRef   =  stateViewControl state
    currentlyTranslating <- (isJust . viewControlTranslateMark) <$> readIORef viewControlRef
    currentlyRotating    <- (isJust . viewControlRotateMark)    <$> readIORef viewControlRef
    go realTime viewRef viewPortRef viewControlRef currentlyTranslating currentlyRotating
  where
    key :: Key
    key  = glutKeyToKey glutKey

    keyState :: KeyState
    keyState = glutKeyStateToKeyState glutKeyState

    keyMods :: Modifiers
    keyMods = glutModifiersToModifiers glutKeyMods

    pos = (posX, posY)

    go :: Bool
       -> IORef View
       -> IORef ViewPort
       -> IORef ViewControl
       -> Bool
       -> Bool
       -> IO ()
    go realTime viewRef viewPortRef viewControlRef currentlyTranslating currentlyRotating
        -- restore viewport
        | isCommand commands CRestore key keyMods
        , keyState == Down
        = do restoreView
             updatePicture stateRef

        -- toggle realtime viewport
        | isCommand commands CToggleRealtime key keyMods
        , keyState == Down
        = do state <- readIORef stateRef
             writeIORef (stateRealTime state) (not realTime)
             when (not realTime) $ do
                 viewPort <- readIORef viewPortRef
                 writeIORef viewPortRef (defaultViewPort (viewPortSize viewPort))
                 updatePicture stateRef

        -- zoom in
        | isCommand commands CBumpZoomIn key keyMods
        , keyState == Down
        = do zoomIn
             updatePicture stateRef

        -- zoom out
        | isCommand commands CBumpZoomOut key keyMods
        , keyState == Down
        = do zoomOut
             updatePicture stateRef

        -- bump left
        | isCommand commands CBumpLeft key keyMods
        , keyState == Down
        = do translateBy realTime viewRef viewPortRef viewControlRef (20, 0)
             updatePicture stateRef

        -- bump right
        | isCommand commands CBumpRight key keyMods
        , keyState == Down
        = do translateBy realTime viewRef viewPortRef viewControlRef (-20, 0)
             updatePicture stateRef

        -- bump up
        | isCommand commands CBumpUp key keyMods
        , keyState == Down
        = do translateBy realTime viewRef viewPortRef viewControlRef (0, 20)
             updatePicture stateRef

        -- bump down
        | isCommand commands CBumpDown key keyMods
        , keyState == Down
        = do translateBy realTime viewRef viewPortRef viewControlRef (0, -20)
             updatePicture stateRef

        -- bump clockwise
        | isCommand commands CBumpClockwise key keyMods
        , keyState == Down
        = do rotateBy realTime viewRef viewPortRef viewControlRef (+ 5)
             updatePicture stateRef

        -- bump counter-clockwise
        | isCommand commands CBumpCClockwise key keyMods
        , keyState == Down
        = do rotateBy realTime viewRef viewPortRef viewControlRef (subtract 5)
             updatePicture stateRef

        -- start translation
        | isCommand commands CTranslate key keyMods
        , keyState == Down
        , not currentlyRotating
        = do modifyIORef viewControlRef $ \s -> s { viewControlTranslateMark = Just pos }
             updatePicture stateRef

        -- end translation
        | currentlyTranslating
        , keyState == Up
        = do modifyIORef viewControlRef $ \s -> s { viewControlTranslateMark = Nothing }
             updatePicture stateRef

        -- start rotation
        | isCommand commands CRotate key keyMods
        , keyState == Down
        , not currentlyTranslating
        = do modifyIORef viewControlRef $ \s -> s { viewControlRotateMark = Just pos }
             updatePicture stateRef

        -- end rotation
        | currentlyRotating
        , keyState == Up
        = do modifyIORef viewControlRef $ \s -> s { viewControlRotateMark = Nothing }
             updatePicture stateRef

        -- default
        | otherwise = return ()
      where
        restoreView :: IO ()
        restoreView = do
            defaultView <- stateDefaultView <$> readIORef stateRef
            writeIORef viewRef defaultView
            modifyIORef viewPortRef $ \s ->
                defaultViewPort (viewPortSize s)

        zoomIn :: IO ()
        zoomIn = do
            scaleStep <- viewControlScaleStep <$> readIORef viewControlRef
            scaleBy realTime viewRef viewPortRef viewControlRef scaleStep

        zoomOut :: IO ()
        zoomOut = do
            scaleStep <- viewControlScaleStep <$> readIORef viewControlRef
            scaleBy realTime viewRef viewPortRef viewControlRef (1/scaleStep)

callbackMotion :: IORef State
               -> GLUT.Position
               -> IO ()
callbackMotion stateRef (GLUT.Position posX posY) = do
    state              <- readIORef stateRef
    realTime           <- readIORef (stateRealTime state)
    let viewRef        =  stateView state
    let viewPortRef    =  stateViewPort state
    let viewControlRef =  stateViewControl state
    translateMark      <- viewControlTranslateMark <$> readIORef viewControlRef
    rotateMark         <- viewControlRotateMark    <$> readIORef viewControlRef
    case translateMark of
      Nothing   -> return ()
      Just mark -> do motionTranslate realTime viewRef viewPortRef viewControlRef mark (posX, posY)
                      updatePicture stateRef
    case rotateMark of
      Nothing   -> return ()
      Just mark -> do motionRotate realTime viewRef viewPortRef viewControlRef mark (posX, posY)
                      updatePicture stateRef

motionTranslate :: Bool
                -> IORef View
                -> IORef ViewPort
                -> IORef ViewControl
                -> (GL.GLint, GL.GLint)
                -> (GL.GLint, GL.GLint)
                -> IO ()
motionTranslate realTime viewRef viewPortRef viewControlRef (markX, markY) pos@(posX, posY) = do
    let dX = fromIntegral $ markX - posX
    let dY = fromIntegral $ markY - posY
    translateBy realTime viewRef viewPortRef viewControlRef (dX, dY)

    modifyIORef viewControlRef $ \s ->
        s { viewControlTranslateMark = Just pos }

motionRotate :: Bool
             -> IORef View
             -> IORef ViewPort
             -> IORef ViewControl
             -> (GL.GLint, GL.GLint)
             -> (GL.GLint, GL.GLint)
             -> IO ()
motionRotate realTime viewRef viewPortRef viewControlRef (markX, _) pos@(posX, _) = do
    rotateFactor <- viewControlRotateFactor <$> readIORef viewControlRef

    rotateBy realTime viewRef viewPortRef viewControlRef (+ rotateFactor * fromIntegral (posX - markX))

    modifyIORef viewControlRef $ \s ->
        s { viewControlRotateMark = Just pos }

resizeTo :: Bool
         -> IORef View
         -> IORef ViewPort
         -> IORef ViewControl
         -> (Int, Int)
         -> IO ()
resizeTo _ _ viewPortRef _ size = do
    modifyIORef viewPortRef $ \s ->
        s { viewPortSize = size }

translateBy :: Bool
            -> IORef View
            -> IORef ViewPort
            -> IORef ViewControl
            -> (Float, Float)
            -> IO ()
translateBy realTime viewRef viewPortRef _ (dX, dY) = do
    viewPort             <- readIORef viewPortRef
    let (sizeX, sizeY)   =  viewPortSize      viewPort
        (transX, transY) =  viewPortTranslate viewPort
        scale            =  viewPortScale     viewPort
        r                =  viewPortRotate    viewPort

    let offset           =  (dX / scale, dY / scale)

    let (oX, oY)         =  rotateV (degToRad r) offset

    modifyIORef viewRef $ \view ->
        let width  = right view - left view
            height = top view - bot view
            dX'    = realToFrac (width  * realToFrac (oX    * scale) / fromIntegral sizeX)
            dY'    = realToFrac (height * realToFrac ((-oY) * scale) / fromIntegral sizeY)
        in
          View { left  = left view  + dX'
               , bot   = bot view   + dY'
               , right = right view + dX'
               , top   = top view   + dY'
               }

    when (not realTime) $
        modifyIORef viewPortRef $ \s ->
            s { viewPortTranslate = (transX - oX, transY + oY) }

rotateBy :: Bool
         -> IORef View
         -> IORef ViewPort
         -> IORef ViewControl
         -> (Float -> Float)
         -> IO ()
rotateBy _ _ viewPortRef _ delta = do
    modifyIORef viewPortRef $ \s ->
        s { viewPortRotate = delta (viewPortRotate s) }

scaleBy :: Bool
        -> IORef View
        -> IORef ViewPort
        -> IORef ViewControl
        -> Float
        -> IO ()
scaleBy realTime viewRef viewPortRef _ x = do
    modifyIORef viewRef $ \view ->
        let width   = right view - left view
            height  = top view - bot view
            width'  = width / realToFrac x
            height' = height / realToFrac x
            centerX = (left view + right view) / 2
            centerY = (top view  + bot view)   / 2
        in
          View { left  = centerX - width'/2
               , bot   = centerY - height'/2
               , right = centerX + width'/2
               , top   = centerY + height'/2
               }

    when (not realTime) $
        modifyIORef viewPortRef $ \s ->
            s { viewPortScale = viewPortScale s * x }

updatePicture :: IORef State -> IO ()
updatePicture stateRef = do
    state    <- readIORef stateRef
    realTime <- readIORef (stateRealTime state)
    viewPort <- readIORef (stateViewPort state)
    when realTime $ do
        let (width, height) = (viewPortSize viewPort)
            dim             = min width height
        view <- readIORef (stateView state)
        pic' <- stateFrameGen state 0 view (dim, dim)
        writeIORef (statePicture state)  pic'
    GLUT.postRedisplay Nothing

draw :: IORef State
     -> IO ()
draw stateRef = do
  state    <- readIORef stateRef
  viewPort <- readIORef (stateViewPort state)
  pic      <- readIORef (statePicture state)
  withViewPort viewPort $ do
      setLineSmooth (stateLineSmooth state)
      setBlendAlpha (stateBlendAlpha state)
      drawPicture pic
  where
    drawPicture :: Picture -> IO ()
    drawPicture (Bitmap width height (BitmapData _ fptr)) =
        go width height (HostTexSource fptr)

    drawPicture (PBO width height pbo) =
        go width height (PBOTexSource pbo)

    go :: Int-> Int -> TextureSource -> IO ()
    go width height texSource = do
        tex <- loadTexture width height texSource

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
    let (sizeX, sizeY) = viewPortSize port
    GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral sizeX) (fromIntegral sizeY))
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

data TextureSource = HostTexSource (ForeignPtr Word8)
                   | PBOTexSource GL.BufferObject

loadTexture  :: Int
             -> Int
             -> TextureSource
             -> IO Texture
loadTexture width height texSource = do
    -- Allocate texture handle for texture
    [tex] <- GL.genObjectNames 1
    GL.textureBinding GL.Texture2D $= Just tex

    loadTexSource texSource

    return Texture { texObject = tex }
  where
    loadTexSource :: TextureSource -> IO ()
    -- Sets the texture in bitmapData as the current texture This copies the
    -- data from the pointer into OpenGL texture memory, so it's ok if the
    -- foreignptr gets garbage collected after this.
    loadTexSource (HostTexSource fptr) =
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

    loadTexSource (PBOTexSource pbo) = do
        GL.bindBuffer GL.PixelUnpackBuffer $= Just pbo

        GL.texImage2D
                Nothing
                GL.NoProxy
                0
                GL.RGBA8
                (GL.TextureSize2D
                        (gsizei width)
                        (gsizei height))
                0
                (GL.PixelData GL.RGBA GL.UnsignedInt8888 nullPtr)

        GL.bindBuffer GL.PixelUnpackBuffer $= Nothing

freeTexture :: Texture -> IO ()
freeTexture tex = GL.deleteObjectNames [texObject tex]

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

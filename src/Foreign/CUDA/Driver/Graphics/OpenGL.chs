{-# LANGUAGE ForeignFunctionInterface #-}

-- |
-- Module    : Foreign.CUDA.Driver.Graphics.OpenGL
-- Copyright : (c) 2012 Geoffrey Mainland
-- License   : BSD
--
-- OpenGL graphics interoperability for low-level driver interface
--

module Foreign.CUDA.Driver.Graphics.OpenGL (

  -- * OpenGL graphics interoperability
  DeviceList(..),

  createGLContext, getGLDevices, registerBuffer
) where

#include <cuda.h>
#include <GL/gl.h>
#include <cudaGL.h>
{# context lib="cuda" #}

import Control.Monad (liftM)
import Unsafe.Coerce

import Graphics.Rendering.OpenGL.GL

import Foreign
import Foreign.C
import Foreign.CUDA.Driver.Context
import Foreign.CUDA.Driver.Device
import Foreign.CUDA.Driver.Error
import Foreign.CUDA.Driver.Graphics
import Foreign.CUDA.Internal.C2HS

instance Storable Device where
  sizeOf    (Device dev) = sizeOf dev
  alignment (Device dev) = alignment dev
  poke p    (Device dev) = poke (castPtr p) dev
  peek p                 = Device `liftM` peek (castPtr p)

-- | CUDA devices corresponding to an OpenGL device.
{# enum CUGLDeviceList as DeviceList
  { underscoreToCase }
  with prefix="CU_GL" deriving (Eq, Show) #}

-- | Create a new CUDA context, initialized for OpenGL interoperability, and
--  associate it with the calling thread
createGLContext :: Device -> [ContextFlag] -> IO Context
createGLContext dev flags = resultIfOk =<< cuGLCtxCreate_v2 flags dev

{# fun unsafe cuGLCtxCreate_v2
  { alloca-         `Context'       peekCtx*
  , combineBitMasks `[ContextFlag]'
  , useDevice       `Device'                 } -> `Status' cToEnum #}
  where peekCtx = liftM Context . peek

-- | Gets the CUDA devices associated with the current OpenGL context.
-- getGLDevices

getGLDevices :: Int -> DeviceList -> IO [Device]
getGLDevices maxDevices dlist =
    allocaArray maxDevices $ \devs_ptr -> do
    count <- resultIfOk =<< cuGLGetDevices devs_ptr (fromIntegral maxDevices) dlist
    peekArray (fromIntegral count) devs_ptr

{# fun unsafe cuGLGetDevices
  { alloca-   `CUInt'      peekIntConv*
  , castPtr   `Ptr Device'
  , id        `CUInt'
  , cFromEnum `DeviceList'              } -> `Status' cToEnum #}

-- | Registers an OpenGL buffer object
registerBuffer :: BufferObject -> RegisterFlag -> IO Resource
registerBuffer bo flag =
    resultIfOk =<< cuGraphicsGLRegisterBuffer (unBufferObject bo) flag
  where
    unBufferObject (BufferObject bufferID) = bufferID

{# fun unsafe cuGraphicsGLRegisterBuffer
  { alloca-    `Resource'      peekResource*
  , fromGLuint `GLuint'
  , cFromEnum  `RegisterFlag'                } -> `Status' cToEnum #}
  where
    peekResource = liftM Resource . peek

{-
-- | Register an OpenGL texture or renderbuffer object
{# fun unsafe cuGraphicsGLRegisterImage
  { alloca-      `Resource'      peekResource*
  , fromIntegral `GLuint'
  , fromGLenum   `GLenum'
  , cFromEnum    `RegisterFlag'                } -> `Status' cToEnum #}
  where
    peekResource = liftM Resource . peek

fromGLenum :: GLenum -> CUInt
{-# INLINE fromGLenum #-}
fromGLenum x = unsafeCoerce x
-}

fromGLuint :: GLuint -> CUInt
{-# INLINE fromGLuint #-}
fromGLuint x = unsafeCoerce x

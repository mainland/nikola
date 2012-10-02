{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module    : Foreign.CUDA.Driver.Graphics
-- Copyright : (c) 2012 Geoffrey Mainland
-- License   : BSD
--
-- Graphics interoperability for low-level driver interface
--

module Foreign.CUDA.Driver.Graphics (

  -- * Graphics Resource Management
  RegisterFlag(..), MapResourceFlag(..), Resource(..),

  mapResources, unmapResources, setMapFlags, getMappedPointer, unregisterResource
) where

import Data.Maybe (fromMaybe)

import Foreign
import Foreign.C
import Foreign.CUDA.Ptr
import Foreign.CUDA.Driver.Error
import Foreign.CUDA.Driver.Marshal
import Foreign.CUDA.Driver.Stream (Stream(..))
import Foreign.CUDA.Internal.C2HS

#include <cuda.h>
{# context lib="cuda" #}

-- | Flags to register a graphics resource
{# enum CUgraphicsRegisterFlags as RegisterFlag
  { underscoreToCase
  , NONE           as RegisterNone
  , READ_ONLY      as RegisterReadOnly
  , WRITE_DISCARD  as RegisterWriteDiscard
  , SURFACE_LDST   as RegisterSurfaceLdst
  , TEXTURE_GATHER as RegisterGather }
  with prefix="CU_GRAPHICS_REGISTER_FLAGS" deriving (Eq, Show) #}

-- | Flags for mapping and unmapping interop resources
{# enum CUgraphicsMapResourceFlags as MapResourceFlag
  { underscoreToCase
  , NONE           as MapResourceNone
  , READ_ONLY      as MapResourceReadOnly
  , WRITE_DISCARD  as MapResourceWriteDiscard }
  with prefix="CU_GRAPHICS_MAP_RESOURCE_FLAGS" deriving (Eq, Show) #}

-- | A graphics resource
newtype Resource = Resource { useResource :: {# type CUgraphicsResource #}}
  deriving (Storable)

-- | Maps resources for access by CUDA. The resources may be accessed by CUDA
-- until they are unmapped. The graphics API from which the resources were
-- registered should not access any resources while they are mapped by CUDA. If
-- an application does so, the results are undefined.
--
-- This function provides the synchronization guarantee that any graphics calls
-- issued before 'mapResources' will complete before any subsequent CUDA work
-- issued in the specified stream begins.

mapResources :: [Resource] -> Maybe Stream -> IO ()
mapResources res maybe_stream =
    withArray res $ \res_ptr -> do
    nothingIfOk =<< cuGraphicsMapResources count res_ptr stream
  where
    count  = length res
    stream = fromMaybe (Stream nullPtr) maybe_stream

{# fun unsafe cuGraphicsMapResources
  {           `Int'
  , castPtr   `Ptr (Resource)'
  , useStream `Stream'
  } -> `Status' cToEnum #}

-- | Unmaps the graphics resources.
--
-- Once unmapped, the resources may not be accessed by CUDA until they are
-- mapped again.
--
-- This function provides the synchronization guarantee that any CUDA work
-- issued in the specified stream before 'unmapResources' will complete before
-- any subsequently issued graphics work begins.

unmapResources :: [Resource] -> Maybe Stream -> IO ()
unmapResources res maybe_stream =
    withArray res $ \res_ptr -> do
    nothingIfOk =<< cuGraphicsUnmapResources count res_ptr stream
  where
    count  = length res
    stream = fromMaybe (Stream nullPtr) maybe_stream

{# fun unsafe cuGraphicsUnmapResources
  {           `Int'
  , castPtr   `Ptr (Resource)'
  , useStream `Stream'
  } -> `Status' cToEnum #}

-- | Set usage flags for mapping a graphics resource.

setMapFlags :: Resource -> [MapResourceFlag] -> IO ()
setMapFlags res flags =
    nothingIfOk =<< cuGraphicsResourceSetMapFlags res flags

{# fun unsafe cuGraphicsResourceSetMapFlags
  { useResource     `Resource'
  , combineBitMasks `[MapResourceFlag]'
  } -> `Status' cToEnum #}

-- |Get a device pointer through which to access a mapped graphics resource.
--
-- Returns a pointer through which the mapped graphics resource may be accessed
-- and the size of the memory in bytes which may be accessed from that
-- pointer. The returned pointer may change every time the resource is mapped.

getMappedPointer :: Resource -> IO (DevicePtr a, Int)
getMappedPointer res = do
    (status, dptr, size) <- cuGraphicsResourceGetMappedPointer_v2 res
    resultIfOk (status, (dptr, fromIntegral size))

#if CUDA_VERSION >= 3020
{# fun unsafe cuGraphicsResourceGetMappedPointer_v2
  { alloca-     `DevicePtr a' peekDeviceHandle*
  , alloca-     `CSize'       peekIntConv*
  , useResource `Resource'
  } -> `Status' cToEnum #}
#endif /* CUDA_VERSION >= 3020 */

{-
CUresult CUDAAPI cuGraphicsSubResourceGetMappedArray(CUarray *pArray, CUgraphicsResource resource, unsigned int arrayIndex, unsigned int mipLevel);
-}

-- | Unregisters the graphics resource so it is not accessible by CUDA unless
-- registered again.
--
unregisterResource :: Resource -> IO ()
unregisterResource res = nothingIfOk =<< cuGraphicsUnregisterResource res

{# fun unsafe cuGraphicsUnregisterResource
  { useResource `Resource' } -> `Status' cToEnum #}

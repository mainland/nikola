-- Copyright (c) 2009
--         The President and Fellows of Harvard College.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions
-- are met:
-- 1. Redistributions of source code must retain the above copyright
--    notice, this list of conditions and the following disclaimer.
-- 2. Redistributions in binary form must reproduce the above copyright
--    notice, this list of conditions and the following disclaimer in the
--    documentation and/or other materials provided with the distribution.
-- 3. Neither the name of the University nor the names of its contributors
--    may be used to endorse or promote products derived from this software
--    without specific prior written permission.

-- THIS SOFTWARE IS PROVIDED BY THE UNIVERSITY AND CONTRIBUTORS ``AS IS'' AND
-- ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
-- IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
-- ARE DISCLAIMED.  IN NO EVENT SHALL THE UNIVERSITY OR CONTRIBUTORS BE LIABLE
-- FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
-- OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
-- HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
-- LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
-- OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
-- SUCH DAMAGE.

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module CUDA.Internal where

import Control.Applicative
import Control.Exception
import Control.Monad (liftM)
import Data.ByteString
import Data.ByteString.Internal
import Data.ByteString.Unsafe
import Data.Typeable
import Foreign
import Foreign.C.String
import Foreign.C.Types

#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

#include <cuda.h>

-- |CUDA device pointer
newtype CUDevicePtr a = CUDevicePtr { unCUDevicePtr :: Ptr #{type CUdeviceptr} }
  deriving (Eq, Ord, Show)

castCUDevicePtr :: CUDevicePtr a -> CUDevicePtr b
castCUDevicePtr = CUDevicePtr . unCUDevicePtr

-- |CUDA device
newtype CUDevice = CUDevice { unCUDevice :: #{type CUdevice} }
  deriving (Eq, Ord, Storable)

instance Show CUDevice where
    show (CUDevice dev) = "Device " ++ show dev

-- |CUDA modules
data OpaqueModule = OpaqueModule

newtype CUModule = CUModule { unCUModule :: Ptr OpaqueModule }
  deriving (Eq, Ord, Show)

-- |CUDA function
data OpaqueFunction = OpaqueFunction

newtype CUFunction = CUFunction { unCUFunction :: Ptr OpaqueFunction }
  deriving (Eq, Ord, Show, Storable)

-- |CUDA array
data OpaqueArray = OpaqueArray

newtype CUArray = CUArray { unCUArray :: Ptr OpaqueArray }

-- |CUDA texture reference
data OpaqueTexRef = OpaqueTexRef

newtype CUTexRef = CUTexRef { unCUTexref :: Ptr OpaqueTexRef }
  deriving (Eq, Ord, Storable)

-- |CUDA event
data OpaqueEvent = OpaqueEvent

newtype CUEvent = CUEvent { unCUEvent :: Ptr OpaqueEvent }

-- |CUDA stream
data OpaqueStream = OpaqueStream

newtype CUStream = CUStream { unCUStream :: Ptr OpaqueStream }

-- |CUDA result codes
data CUResult = CUDASuccess
              | CUDAErrorInvalidValue
              | CUDAErrorOutOfMemory
              | CUDAErrorNotInitialized
              | CUDAErrorDeinitialized

              | CUDAErrorNoDevice
              | CUDAErrorInvalidDevice

              | CUDAErrorInvalidImage
              | CUDAErrorInvalidContext
              | CUDAErrorContextAlreadyCurrent
              | CUDAErrorMapFailed
              | CUDAErrorUnmapFailed
              | CUDAErrorArrayIsMapped
              | CUDAErrorAlreadyMapped
              | CUDAErrorNoBinaryForGPU
              | CUDAErrorAlreadyAcquired
              | CUDAErrorNotMapped

              | CUDAErrorInvalidSource
              | CUDAErrorFileNotFound

              | CUDAErrorInvalidHandle

              | CUDAErrorNotFound

              | CUDAErrorNotReady

              | CUDAErrorLaunchFailed
              | CUDAErrorLaunchOutOfResources
              | CUDAErrorLaunchTimeout
              | CUDAErrorLaunchIncompatibleTexturing

              | CUDAErrorUnknown
  deriving (Eq, Ord, Show)

instance Enum CUResult where
    fromEnum CUDASuccess             = #{const CUDA_SUCCESS}
    fromEnum CUDAErrorInvalidValue   = #{const CUDA_ERROR_INVALID_VALUE}
    fromEnum CUDAErrorOutOfMemory    = #{const CUDA_ERROR_OUT_OF_MEMORY}
    fromEnum CUDAErrorNotInitialized = #{const CUDA_ERROR_NOT_INITIALIZED}
    fromEnum CUDAErrorDeinitialized  = #{const CUDA_ERROR_DEINITIALIZED}

    fromEnum CUDAErrorNoDevice      = #{const CUDA_ERROR_NO_DEVICE}
    fromEnum CUDAErrorInvalidDevice = #{const CUDA_ERROR_INVALID_DEVICE}

    fromEnum CUDAErrorInvalidImage          = #{const CUDA_ERROR_INVALID_IMAGE}
    fromEnum CUDAErrorInvalidContext        = #{const CUDA_ERROR_INVALID_CONTEXT}
    fromEnum CUDAErrorContextAlreadyCurrent = #{const CUDA_ERROR_CONTEXT_ALREADY_CURRENT}
    fromEnum CUDAErrorMapFailed             = #{const CUDA_ERROR_MAP_FAILED}
    fromEnum CUDAErrorUnmapFailed           = #{const CUDA_ERROR_UNMAP_FAILED}
    fromEnum CUDAErrorArrayIsMapped         = #{const CUDA_ERROR_ARRAY_IS_MAPPED}
    fromEnum CUDAErrorAlreadyMapped         = #{const CUDA_ERROR_ALREADY_MAPPED}
    fromEnum CUDAErrorNoBinaryForGPU        = #{const CUDA_ERROR_NO_BINARY_FOR_GPU}
    fromEnum CUDAErrorAlreadyAcquired       = #{const CUDA_ERROR_ALREADY_ACQUIRED}
    fromEnum CUDAErrorNotMapped             = #{const CUDA_ERROR_NOT_MAPPED}

    fromEnum CUDAErrorInvalidSource = #{const CUDA_ERROR_INVALID_SOURCE}
    fromEnum CUDAErrorFileNotFound  = #{const CUDA_ERROR_FILE_NOT_FOUND}

    fromEnum CUDAErrorInvalidHandle = #{const CUDA_ERROR_INVALID_HANDLE}

    fromEnum CUDAErrorNotFound = #{const CUDA_ERROR_NOT_FOUND}

    fromEnum CUDAErrorNotReady = #{const CUDA_ERROR_NOT_READY}

    fromEnum CUDAErrorLaunchFailed =
        #{const CUDA_ERROR_LAUNCH_FAILED}
    fromEnum CUDAErrorLaunchOutOfResources =
        #{const CUDA_ERROR_LAUNCH_OUT_OF_RESOURCES}
    fromEnum CUDAErrorLaunchTimeout =
        #{const CUDA_ERROR_LAUNCH_TIMEOUT}
    fromEnum CUDAErrorLaunchIncompatibleTexturing =
        #{const CUDA_ERROR_LAUNCH_INCOMPATIBLE_TEXTURING}

    fromEnum CUDAErrorUnknown = #{const CUDA_ERROR_UNKNOWN}

    toEnum #{const CUDA_SUCCESS}               = CUDASuccess
    toEnum #{const CUDA_ERROR_INVALID_VALUE}   = CUDAErrorInvalidValue
    toEnum #{const CUDA_ERROR_OUT_OF_MEMORY}   = CUDAErrorOutOfMemory
    toEnum #{const CUDA_ERROR_NOT_INITIALIZED} = CUDAErrorNotInitialized
    toEnum #{const CUDA_ERROR_DEINITIALIZED}   = CUDAErrorDeinitialized

    toEnum #{const CUDA_ERROR_NO_DEVICE}      = CUDAErrorNoDevice
    toEnum #{const CUDA_ERROR_INVALID_DEVICE} = CUDAErrorInvalidDevice

    toEnum #{const CUDA_ERROR_INVALID_IMAGE}           = CUDAErrorInvalidImage
    toEnum #{const CUDA_ERROR_INVALID_CONTEXT}         = CUDAErrorInvalidContext
    toEnum #{const CUDA_ERROR_CONTEXT_ALREADY_CURRENT} = CUDAErrorContextAlreadyCurrent
    toEnum #{const CUDA_ERROR_MAP_FAILED}              = CUDAErrorMapFailed
    toEnum #{const CUDA_ERROR_UNMAP_FAILED}            = CUDAErrorUnmapFailed
    toEnum #{const CUDA_ERROR_ARRAY_IS_MAPPED}         = CUDAErrorArrayIsMapped
    toEnum #{const CUDA_ERROR_ALREADY_MAPPED}          = CUDAErrorAlreadyMapped
    toEnum #{const CUDA_ERROR_NO_BINARY_FOR_GPU}       = CUDAErrorNoBinaryForGPU
    toEnum #{const CUDA_ERROR_ALREADY_ACQUIRED}        = CUDAErrorAlreadyAcquired
    toEnum #{const CUDA_ERROR_NOT_MAPPED}              = CUDAErrorNotMapped

    toEnum #{const CUDA_ERROR_INVALID_SOURCE} = CUDAErrorInvalidSource
    toEnum #{const CUDA_ERROR_FILE_NOT_FOUND} = CUDAErrorFileNotFound

    toEnum #{const CUDA_ERROR_INVALID_HANDLE} = CUDAErrorInvalidHandle

    toEnum #{const CUDA_ERROR_NOT_FOUND} = CUDAErrorNotFound

    toEnum #{const CUDA_ERROR_NOT_READY} = CUDAErrorNotReady

    toEnum #{const CUDA_ERROR_LAUNCH_FAILED} =
        CUDAErrorLaunchFailed
    toEnum #{const CUDA_ERROR_LAUNCH_OUT_OF_RESOURCES} =
        CUDAErrorLaunchOutOfResources
    toEnum #{const CUDA_ERROR_LAUNCH_TIMEOUT} =
        CUDAErrorLaunchTimeout
    toEnum #{const CUDA_ERROR_LAUNCH_INCOMPATIBLE_TEXTURING} =
        CUDAErrorLaunchIncompatibleTexturing

    toEnum _ = CUDAErrorUnknown

instance Storable CUResult where
    alignment _ = alignment (undefined :: #{type CUresult})

    sizeOf _ = sizeOf (undefined :: #{type CUresult})

    peek ptr = liftM (toEnum . fromIntegral) (peek (castPtr ptr) :: IO #{type CUresult})

    poke ptr x = poke (castPtr ptr) ((fromIntegral . fromEnum) x :: #{type CUresult})

-- |CUDA context creation flags
newtype CUCtxFlag = CUCtxFlag { unCUCtxFlag :: CUInt }
  deriving (Eq, Ord, Show, Num, Bits)

#{enum CUCtxFlag, CUCtxFlag
 , cuCtxSchedDefault = 0

 , cuCtxSchedAuto  = CU_CTX_SCHED_AUTO
 , cuCtxSchedSpin  = CU_CTX_SCHED_SPIN
 , cuCtxSchedYield = CU_CTX_SCHED_YIELD

 , cuCtxBlockingSync        = CU_CTX_BLOCKING_SYNC
 , cuCtxMapHost             = CU_CTX_MAP_HOST
#if CUDA_VERSION >= 2030
 , cuCtxLocalMemResizeToMax = CU_CTX_LMEM_RESIZE_TO_MAX
#endif
 }

-- |CUDA contexts
data OpaqueContext = OpaqueContext

newtype CUContext = CUContext (Ptr OpaqueContext)
  deriving (Eq)

-- |CUDA device attributes
newtype CUDeviceAttribute =
    CUDeviceAttribute { unCUDeviceAttribute :: #type CUdevice_attribute }
  deriving (Eq, Ord)

#{enum CUDeviceAttribute, CUDeviceAttribute
 , cuDeviceAttributeMaxThreadsPerBlock      = CU_DEVICE_ATTRIBUTE_MAX_THREADS_PER_BLOCK
 , cuDeviceAttributeMaxBlockDimX            = CU_DEVICE_ATTRIBUTE_MAX_BLOCK_DIM_X
 , cuDeviceAttributeMaxBlockDimY            = CU_DEVICE_ATTRIBUTE_MAX_BLOCK_DIM_Y
 , cuDeviceAttributeMaxBlockDimZ            = CU_DEVICE_ATTRIBUTE_MAX_BLOCK_DIM_Z
 , cuDeviceAttributeMaxGridDimX             = CU_DEVICE_ATTRIBUTE_MAX_GRID_DIM_X
 , cuDeviceAttributeMaxGridDimY             = CU_DEVICE_ATTRIBUTE_MAX_GRID_DIM_Y
 , cuDeviceAttributeMaxGridDimZ             = CU_DEVICE_ATTRIBUTE_MAX_GRID_DIM_Z
 , cuDeviceAttributeMaxSharedMemoryPerBLock = CU_DEVICE_ATTRIBUTE_MAX_SHARED_MEMORY_PER_BLOCK
 , cuDeviceAttributeSharedMemoryPerBlock    = CU_DEVICE_ATTRIBUTE_SHARED_MEMORY_PER_BLOCK
 , cuDeviceAttributeTotalConstantMemory     = CU_DEVICE_ATTRIBUTE_TOTAL_CONSTANT_MEMORY
 , cuDeviceAttributeWarpSize                = CU_DEVICE_ATTRIBUTE_WARP_SIZE
 , cuDeviceAttributeMaxPitch                = CU_DEVICE_ATTRIBUTE_MAX_PITCH
 , cuDeviceAttributeMaxRegistersPerBlock    = CU_DEVICE_ATTRIBUTE_MAX_REGISTERS_PER_BLOCK
 , cuDeviceAttributeRegistersPerBlock       = CU_DEVICE_ATTRIBUTE_REGISTERS_PER_BLOCK
 , cuDeviceAttributeClockRate               = CU_DEVICE_ATTRIBUTE_CLOCK_RATE
 , cuDeviceAttributeTextureAlignment        = CU_DEVICE_ATTRIBUTE_TEXTURE_ALIGNMENT

 , cuDeviceAttributeGPUOverlap          = CU_DEVICE_ATTRIBUTE_GPU_OVERLAP
 , cuDeviceAttributeMultiprocessorCount = CU_DEVICE_ATTRIBUTE_MULTIPROCESSOR_COUNT
 , cuDeviceAttributeKernelExecTimeout   = CU_DEVICE_ATTRIBUTE_KERNEL_EXEC_TIMEOUT
 , cuDeviceAttributeIntegrated          = CU_DEVICE_ATTRIBUTE_INTEGRATED
 , cuDeviceAttributeCanMapHostMemory    = CU_DEVICE_ATTRIBUTE_CAN_MAP_HOST_MEMORY
 , cuDeviceAttributeComputeMode         = CU_DEVICE_ATTRIBUTE_COMPUTE_MODE
 }

-- |CUDA device properties
data CUDevprop = CUDevprop
  {  maxThreadsPerBlock  :: Int
  ,  maxThreadsDim       :: (Int, Int, Int)
  ,  maxGridSize         :: (Int, Int, Int)
  ,  sharedMemPerBlock   :: Int
  ,  totalConstantMemory :: Int
  ,  simdWidth           :: Int
  ,  memPitch            :: Int
  ,  regsPerBlock        :: Int
  ,  clockRate           :: Int
  ,  textureAlign        :: Int
  }
  deriving (Show)

instance Storable CUDevprop where
    alignment _ = #{alignment CUdevprop}

    sizeOf _ = #{size CUdevprop}

    peek ptr = do
        _maxThreadsPerBlock  <- #{peek CUdevprop, maxThreadsPerBlock} ptr
        _maxThreadsDimX      <- #{peek CUdevprop, maxThreadsDim[0]} ptr
        _maxThreadsDimY      <- #{peek CUdevprop, maxThreadsDim[1]} ptr
        _maxThreadsDimZ      <- #{peek CUdevprop, maxThreadsDim[2]} ptr
        _maxGridSizeX        <- #{peek CUdevprop, maxGridSize[0]} ptr
        _maxGridSizeY        <- #{peek CUdevprop, maxGridSize[1]} ptr
        _maxGridSizeZ        <- #{peek CUdevprop, maxGridSize[2]} ptr
        _sharedMemPerBlock   <- #{peek CUdevprop, sharedMemPerBlock} ptr
        _totalConstantMemory <- #{peek CUdevprop, totalConstantMemory} ptr
        _simdWidth           <- #{peek CUdevprop, SIMDWidth} ptr
        _memPitch            <- #{peek CUdevprop, memPitch} ptr
        _regsPerBlock        <- #{peek CUdevprop, regsPerBlock} ptr
        _clockRate           <- #{peek CUdevprop, clockRate} ptr
        _textureAlign        <- #{peek CUdevprop, textureAlign} ptr
        return CUDevprop {  maxThreadsPerBlock  = toInt _maxThreadsPerBlock
                         ,  maxThreadsDim       = (toInt _maxThreadsDimX,
                                                   toInt _maxThreadsDimY,
                                                   toInt _maxThreadsDimZ)
                         ,  maxGridSize         = (toInt _maxGridSizeX,
                                                   toInt _maxGridSizeY,
                                                   toInt _maxGridSizeZ)
                         ,  sharedMemPerBlock   = toInt _sharedMemPerBlock
                         ,  totalConstantMemory = toInt _totalConstantMemory
                         ,  simdWidth           = toInt _simdWidth
                         ,  memPitch            = toInt _memPitch
                         ,  regsPerBlock        = toInt _regsPerBlock
                         ,  clockRate           = toInt _clockRate
                         ,  textureAlign        = toInt _textureAlign
                         }
      where
        toInt :: CInt -> Int
        toInt = fromIntegral

    poke ptr devprop = do
        #{poke CUdevprop, maxThreadsPerBlock} ptr (fromInt (maxThreadsPerBlock devprop))
        let (x, y, z) = maxThreadsDim devprop
        #{poke CUdevprop, maxThreadsDim[0]} ptr (fromInt x)
        #{poke CUdevprop, maxThreadsDim[1]} ptr (fromInt y)
        #{poke CUdevprop, maxThreadsDim[2]} ptr (fromInt z)
        let (x, y, z) = maxGridSize devprop
        #{poke CUdevprop, maxGridSize[0]} ptr (fromInt x)
        #{poke CUdevprop, maxGridSize[1]} ptr (fromInt y)
        #{poke CUdevprop, maxGridSize[2]} ptr (fromInt z)
        #{poke CUdevprop, sharedMemPerBlock} ptr (fromInt (sharedMemPerBlock devprop))
        #{poke CUdevprop, totalConstantMemory} ptr (fromInt (totalConstantMemory devprop))
        #{poke CUdevprop, SIMDWidth} ptr (fromInt (simdWidth devprop))
        #{poke CUdevprop, memPitch} ptr (fromInt (memPitch devprop))
        #{poke CUdevprop, regsPerBlock} ptr (fromInt (regsPerBlock devprop))
        #{poke CUdevprop, clockRate} ptr (fromInt (clockRate devprop))
        #{poke CUdevprop, textureAlign} ptr (fromInt (textureAlign devprop))
      where
        fromInt :: Int -> CInt
        fromInt = fromIntegral

-- |CUDA function attributes
newtype CUFunctionAttribute =
    CUFunctionAttribute { unCUFunctionAttribute :: #type CUfunction_attribute }
  deriving (Eq, Ord)

#{enum CUFunctionAttribute, CUFunctionAttribute
 , cuFuncAttributeMaxThreadsperBlock = CU_FUNC_ATTRIBUTE_MAX_THREADS_PER_BLOCK
 , cuFuncAttributeSharedSizeBytes    = CU_FUNC_ATTRIBUTE_SHARED_SIZE_BYTES
 , cuFuncAttributeConstSizeBytes     = CU_FUNC_ATTRIBUTE_CONST_SIZE_BYTES
 , cuFuncAttributeLocalSizeBytes     = CU_FUNC_ATTRIBUTE_LOCAL_SIZE_BYTES
 , cuFuncAttributeNumRegs            = CU_FUNC_ATTRIBUTE_NUM_REGS
 }

-- |CUDA extensible exception
newtype CUException = CUException CUResult
  deriving (Show, Typeable)

instance Exception CUException

checkCUResult :: CInt -> IO ()
checkCUResult i =
    case toEnum (fromIntegral i) of
      CUDASuccess -> return ()
      res ->         throwIO (CUException res)

-- Initialization
foreign import ccall "cuda.h cuInit"
    c_cuInit :: CUInt -> IO CInt

-- Driver version query
foreign import ccall "cuda.h cuDriverGetVersion"
    c_cuDriverGetVersion :: Ptr CInt -> IO CInt

-- Device management
foreign import ccall "cuda.h cuDeviceGet"
    c_cuDeviceGet :: Ptr CUDevice -> CInt -> IO CInt

foreign import ccall "cuda.h cuDeviceGetCount"
    c_cuDeviceGetCount :: Ptr CInt -> IO CInt

foreign import ccall "cuda.h cuDeviceGetName"
    c_cuDeviceGetName :: Ptr CChar -> CInt -> CUDevice -> IO CInt

foreign import ccall "cuda.h cuDeviceComputeCapability"
    c_cuDeviceComputeCapability :: Ptr CInt -> Ptr CInt -> CUDevice -> IO CInt

foreign import ccall "cuda.h cuDeviceTotalMem"
    c_cuDeviceTotalMem :: Ptr CInt -> CUDevice -> IO CInt

foreign import ccall "cuda.h cuDeviceGetProperties"
    c_cuDeviceGetProperties :: Ptr CUDevprop -> CUDevice -> IO CInt

foreign import ccall "cuda.h cuDeviceGetAttribute"
    c_cuDeviceGetAttribute :: Ptr CInt -> CUDeviceAttribute -> CUDevice -> IO CInt

-- Context management
foreign import ccall "cuda.h cuCtxCreate"
    c_cuCtxCreate :: Ptr (Ptr OpaqueContext) -> CUInt -> CUDevice -> IO CInt

foreign import ccall "cuda.h cuCtxDestroy"
    c_cuCtxDestroy :: Ptr OpaqueContext -> IO CInt

foreign import ccall "cuda.h cuCtxAttach"
    c_cuCtxAttach :: Ptr (Ptr OpaqueContext) -> IO CInt

foreign import ccall "cuda.h cuCtxDetach"
    c_cuCtxDetach :: Ptr OpaqueContext -> IO CInt

foreign import ccall "cuda.h cuCtxGetDevice"
    c_cuCtxGetDevice :: Ptr CUDevice -> IO CInt

foreign import ccall "cuda.h cuCtxPushCurrent"
    c_cuCtxPushCurrent :: Ptr OpaqueContext -> IO CInt

foreign import ccall "cuda.h cuCtxPopCurrent"
    c_cuCtxPopCurrent :: Ptr (Ptr OpaqueContext) -> IO CInt

foreign import ccall "cuda.h cuCtxSynchronize"
    c_cuCtxSynchronize :: IO CInt

-- Module management
foreign import ccall "cuda.h cuModuleLoad"
    c_cuModuleLoad :: Ptr (Ptr OpaqueModule) -> CString -> IO CInt

foreign import ccall "cuda.h cuModuleLoadData"
    c_cuModuleLoadData :: Ptr (Ptr OpaqueModule) -> Ptr Word8 -> IO CInt

foreign import ccall "cuda.h cuModuleUnload"
    c_cuModuleUnload :: Ptr OpaqueModule -> IO CInt

foreign import ccall "cuda.h cuModuleGetFunction"
    c_cuModuleGetFunction :: Ptr (Ptr OpaqueFunction) -> Ptr OpaqueModule
                          -> CString -> IO CInt

foreign import ccall "cuda.h cuModuleGetGlobal"
    c_cuModuleGetGlobal :: Ptr #{type CUdeviceptr} -> Ptr CUInt -> Ptr OpaqueModule
                        -> CString -> IO CInt

foreign import ccall "cuda.h cuModuleGetTexRef"
    c_cuModuleGetTexRef :: Ptr (Ptr OpaqueTexRef) -> Ptr OpaqueModule
                        -> CString -> IO CInt

-- Memory management
foreign import ccall "cuda.h cuMemGetInfo"
    c_cuMemGetInfo :: Ptr CUInt -> Ptr CUInt -> IO CInt

foreign import ccall "cuda.h cuMemAlloc"
    c_cuMemAlloc :: Ptr #{type CUdeviceptr} -> CUInt -> IO CInt

foreign import ccall "cuda.h cuMemAllocPitch"
    c_cuMemAllocPitch :: Ptr #{type CUdeviceptr} -> Ptr CUInt -> CUInt -> CUInt
                      -> CUInt -> IO CInt

foreign import ccall "cuda.h cuMemFree"
    c_cuMemFree :: #{type CUdeviceptr} -> IO CInt

foreign import ccall "cuda.h cuMemGetAddressRange"
    c_cuMemGetAddressRange :: Ptr #{type CUdeviceptr} -> Ptr CUInt
                           -> #{type CUdeviceptr} -> IO CInt

-- Synchronous memcpy
foreign import ccall "cuda.h cuMemcpyHtoD"
    c_cuMemcpyHtoD :: #{type CUdeviceptr} -> Ptr a -> CUInt -> IO CInt

foreign import ccall "cuda.h cuMemcpyDtoH"
    c_cuMemcpyDtoH :: Ptr a -> #{type CUdeviceptr} -> CUInt -> IO CInt

-- Function management
foreign import ccall "cuda.h cuFuncSetBlockShape"
    c_cuFuncSetBlockShape :: Ptr OpaqueFunction -> CInt -> CInt -> CInt -> IO CInt

foreign import ccall "cuda.h cuFuncSetSharedSize"
    c_cuFuncSetSharedSize :: Ptr OpaqueFunction -> CUInt -> IO CInt

foreign import ccall "cuda.h cuFuncGetAttribute"
    c_cuFuncGetAttribute :: Ptr CInt -> CUFunctionAttribute -> Ptr OpaqueFunction -> IO CInt

-- Parameter management
foreign import ccall "cuda.h cuParamSetSize"
    c_cuParamSetSize :: Ptr OpaqueFunction -> CUInt -> IO CInt

foreign import ccall "cuda.h cuParamSeti"
    c_cuParamSeti :: Ptr OpaqueFunction -> CInt -> CUInt -> IO CInt

foreign import ccall "cuda.h cuParamSetf"
    c_cuParamSetf :: Ptr OpaqueFunction -> CInt -> CFloat -> IO CInt

foreign import ccall "cuda.h cuParamSetv"
    c_cuParamSetv :: Ptr OpaqueFunction -> CInt -> Ptr a -> CUInt -> IO CInt

-- Launch functions
foreign import ccall "cuda.h cuLaunch"
    c_cuLaunch :: Ptr OpaqueFunction -> IO CInt

foreign import ccall "cuda.h cuLaunchGrid"
    c_cuLaunchGrid :: Ptr OpaqueFunction -> CInt -> CInt -> IO CInt

foreign import ccall "cuda.h cuLaunchGridAsync"
    c_cuLaunchGridAsync :: Ptr OpaqueFunction -> CInt -> CInt -> Ptr OpaqueStream -> IO CInt

-- Events
foreign import ccall "cuda.h cuEventCreate"
    c_cuEventCreate :: Ptr (Ptr OpaqueEvent) -> CUInt -> IO CInt

foreign import ccall "cuda.h cuEventRecord"
    c_cuEventRecord :: Ptr OpaqueEvent -> Ptr OpaqueStream -> IO CInt

foreign import ccall "cuda.h cuEventQuery"
    c_cuEventQuery :: Ptr OpaqueEvent -> IO CInt

foreign import ccall "cuda.h cuEventSynchronize"
    c_cuEventSynchronize :: Ptr OpaqueEvent -> IO CInt

foreign import ccall "cuda.h cuEventDestroy"
    c_cuEventDestroy :: Ptr OpaqueEvent -> IO CInt

foreign import ccall "cuda.h cuEventElapsedTime"
    c_cuEventElapsedTime :: Ptr CFloat -> Ptr OpaqueEvent -> Ptr OpaqueEvent -> IO CInt

-- Streams
foreign import ccall "cuda.h cuStreamCreate"
    c_cuStreamCreate :: Ptr (Ptr OpaqueStream) -> CUInt -> IO CInt

foreign import ccall "cuda.h cuStreamQuery"
    c_cuStreamQuery :: Ptr OpaqueStream -> IO CInt

foreign import ccall "cuda.h cuStreamSynchronize"
    c_cuStreamSynchronize :: Ptr OpaqueStream -> IO CInt

foreign import ccall "cuda.h cuStreamDestroy"
    c_cuStreamDestroy :: Ptr OpaqueStream -> IO CInt

-- |Utilities
withDevicePtrAddress :: CUDevicePtr a -> (Ptr #{type CUdeviceptr} -> IO b) -> IO b
withDevicePtrAddress (CUDevicePtr devPtr) act =
    act devPtr

withDevicePtr :: CUDevicePtr a -> (#{type CUdeviceptr} -> IO b) -> IO b
withDevicePtr (CUDevicePtr devPtr) act =
    peek devPtr >>= act

-- |Initialization
cuInit :: IO ()
cuInit = c_cuInit 0 >>= checkCUResult

-- |Driver version query
cuDriverGetVersion :: IO Int
cuDriverGetVersion = do
   alloca $ \vers -> do
   c_cuDriverGetVersion vers >>= checkCUResult
   fromIntegral <$> peek vers

-- |Device management
cuDeviceGet :: Int -> IO CUDevice
cuDeviceGet ordinal = do
    alloca $ \devPtr -> do
    c_cuDeviceGet devPtr (fromIntegral ordinal) >>= checkCUResult
    peek devPtr

cuDeviceGetCount :: IO Int
cuDeviceGetCount = do
    alloca $ \countPtr -> do
    c_cuDeviceGetCount countPtr >>= checkCUResult
    fromIntegral <$> peek countPtr

cuDeviceGetName :: CUDevice -> IO String
cuDeviceGetName dev = do
    allocaBytes count $ \namePtr -> do
    c_cuDeviceGetName namePtr count dev >>= checkCUResult
    peekCString namePtr
  where
    count :: Integral a => a
    count = 255

cuDeviceComputeCapability :: CUDevice -> IO (Int, Int)
cuDeviceComputeCapability dev = do
    alloca $ \majorPtr -> do
    alloca $ \minorPtr -> do
    c_cuDeviceComputeCapability majorPtr minorPtr dev >>= checkCUResult
    major <- peek majorPtr
    minor <- peek minorPtr
    return (fromIntegral major, fromIntegral minor)

cuDeviceTotalMem :: CUDevice -> IO Int
cuDeviceTotalMem dev = do
    alloca $ \totalMemPtr -> do
    c_cuDeviceTotalMem totalMemPtr dev >>= checkCUResult
    fromIntegral <$> peek totalMemPtr

cuDeviceGetProperties :: CUDevice -> IO CUDevprop
cuDeviceGetProperties dev = do
    alloca $ \propPtr -> do
    c_cuDeviceGetProperties propPtr dev >>= checkCUResult
    peek propPtr

cuDeviceGetAttribute :: CUDeviceAttribute -> CUDevice -> IO Int
cuDeviceGetAttribute attr dev = do
    alloca $ \valPtr -> do
    c_cuDeviceGetAttribute valPtr attr dev >>= checkCUResult
    fromIntegral <$> peek valPtr

-- |Context management
cuCtxCreate :: CUCtxFlag -> CUDevice -> IO CUContext
cuCtxCreate (CUCtxFlag flags) dev = do
    alloca $ \ctxPtrPtr -> do
    c_cuCtxCreate ctxPtrPtr flags dev >>= checkCUResult
    ctxPtr <- peek ctxPtrPtr
    return (CUContext ctxPtr)

cuCtxDestroy :: CUContext -> IO ()
cuCtxDestroy (CUContext ctxPtr) =
    c_cuCtxDestroy ctxPtr >>= checkCUResult

cuCtxAttach :: IO CUContext
cuCtxAttach = do
    alloca $ \ctxPtrPtr -> do
    c_cuCtxAttach ctxPtrPtr >>= checkCUResult
    ctxPtr <- peek ctxPtrPtr
    return (CUContext ctxPtr)

cuCtxDetach :: CUContext -> IO ()
cuCtxDetach (CUContext ctxPtr) =
    c_cuCtxDetach ctxPtr >>= checkCUResult

cuCtxGetDevice :: IO CUDevice
cuCtxGetDevice = do
    alloca $ \devPtr -> do
    c_cuCtxGetDevice devPtr >>= checkCUResult
    peek devPtr

cuCtxPushCurrent :: CUContext -> IO ()
cuCtxPushCurrent (CUContext ctxPtr) =
    c_cuCtxPushCurrent ctxPtr >>= checkCUResult

cuCtxPopCurrent :: IO CUContext
cuCtxPopCurrent = do
    alloca $ \ctxPtrPtr -> do
    c_cuCtxPopCurrent ctxPtrPtr >>= checkCUResult
    ctxPtr <- peek ctxPtrPtr
    return (CUContext ctxPtr)

cuCtxSynchronize :: IO ()
cuCtxSynchronize =
    c_cuCtxSynchronize >>= checkCUResult

-- |Module management
cuModuleLoad :: String -> IO CUModule
cuModuleLoad fname = do
    withCString fname $ \fnamePtr -> do
    alloca $ \modPtrPtr -> do
    c_cuModuleLoad modPtrPtr fnamePtr >>= checkCUResult
    modPtr <- peek modPtrPtr
    return (CUModule modPtr)

cuModuleLoadData :: ByteString -> IO CUModule
cuModuleLoadData bs = unsafeUseAsCString bs' $ \ptr -> do
    alloca $ \modPtrPtr -> do
    c_cuModuleLoadData modPtrPtr (castPtr ptr) >>= checkCUResult
    modPtr <- peek modPtrPtr
    return (CUModule modPtr)
  where
    bs' = snoc bs (fromIntegral 0)

cuModuleUnload :: CUModule -> IO ()
cuModuleUnload (CUModule modPtr) =
    c_cuModuleUnload modPtr >>= checkCUResult

cuModuleGetFunction :: CUModule -> String -> IO CUFunction
cuModuleGetFunction (CUModule modPtr) symbolName = do
    withCString symbolName $ \symbolNamePtr -> do
    alloca $ \funPtrPtr -> do
    c_cuModuleGetFunction funPtrPtr modPtr symbolNamePtr >>= checkCUResult
    funPtr <- peek funPtrPtr
    return (CUFunction funPtr)

cuModuleGetGlobal :: CUModule -> String -> IO (CUDevicePtr (), Word)
cuModuleGetGlobal (CUModule modPtr) symbolName = do
    withCString symbolName $ \symbolNamePtr -> do
    devPtr <- malloc
    alloca $ \lenPtr -> do
    c_cuModuleGetGlobal devPtr lenPtr modPtr symbolNamePtr >>= checkCUResult
    len <- peek lenPtr
    return (CUDevicePtr devPtr, fromIntegral len)

cuModuleGetTexRef :: CUModule -> String -> IO CUTexRef
cuModuleGetTexRef (CUModule modPtr) tname = do
    withCString tname $ \tnamePtr -> do
    alloca $ \texPtrPtr -> do
    c_cuModuleGetTexRef texPtrPtr modPtr tnamePtr >>= checkCUResult
    texPtr <- peek texPtrPtr
    return (CUTexRef texPtr)

-- |Memory management
cuMemGetInfo :: IO (Word, Word)
cuMemGetInfo = do
    alloca $ \freePtr -> do
    alloca $ \totalPtr -> do
    c_cuMemGetInfo freePtr totalPtr >>= checkCUResult
    free  <- peek freePtr
    total <- peek totalPtr
    return (fromIntegral free, fromIntegral total)

cuMemAlloc :: Int -> IO (CUDevicePtr a)
cuMemAlloc n = do
    devPtr <- malloc
    poke devPtr 0
    c_cuMemAlloc devPtr (fromIntegral n) >>= checkCUResult
    return (CUDevicePtr devPtr)

cuMemAllocPitch :: Int -> Int -> Int -> IO (CUDevicePtr a, Int)
cuMemAllocPitch w h elemSize = do
    devPtr <- malloc
    poke devPtr 0
    alloca $ \pitchPtr -> do
    c_cuMemAllocPitch devPtr pitchPtr
        (fromIntegral w) (fromIntegral h)
        (fromIntegral elemSize) >>= checkCUResult
    pitch <- peek pitchPtr
    return (CUDevicePtr devPtr, fromIntegral pitch)

cuMemFree :: CUDevicePtr a -> IO ()
cuMemFree (CUDevicePtr devPtr) =
    peek devPtr >>= c_cuMemFree >>= checkCUResult

cuMemGetAddressRange :: CUDevicePtr a -> IO (CUDevicePtr a, Int)
cuMemGetAddressRange (CUDevicePtr devPtr) = do
    baseDevPtr <- malloc
    alloca $ \sizePtr -> do
    peek devPtr >>= c_cuMemGetAddressRange baseDevPtr sizePtr >>= checkCUResult
    size <- peek sizePtr
    return (CUDevicePtr baseDevPtr, fromIntegral size)

-- |Synchronous memcpy
cuMemcpyHtoD :: Ptr a -> CUDevicePtr a -> Int -> IO ()
cuMemcpyHtoD hostPtr devPtr n = do
    withDevicePtr devPtr $ \devPtr -> do
    c_cuMemcpyHtoD devPtr hostPtr (fromIntegral n) >>= checkCUResult

cuMemcpyDtoH :: CUDevicePtr a -> Ptr a -> Int -> IO ()
cuMemcpyDtoH devPtr hostPtr n = do
    withDevicePtr devPtr $ \devPtr -> do
    c_cuMemcpyDtoH hostPtr devPtr (fromIntegral n) >>= checkCUResult

-- |Function management
cuFuncSetBlockShape :: CUFunction -> Int -> Int -> Int -> IO ()
cuFuncSetBlockShape (CUFunction funcPtr) x y z =
    c_cuFuncSetBlockShape funcPtr
        (fromIntegral x) (fromIntegral y) (fromIntegral z) >>= checkCUResult

cuFuncSetSharedSize :: CUFunction -> Int -> IO ()
cuFuncSetSharedSize (CUFunction funcPtr) size =
    c_cuFuncSetSharedSize funcPtr (fromIntegral size) >>= checkCUResult

cuFuncGetAttribute :: CUFunction -> CUFunctionAttribute -> IO Int
cuFuncGetAttribute (CUFunction funcPtr) attr = do
    alloca $ \valPtr -> do
    c_cuFuncGetAttribute valPtr attr funcPtr >>= checkCUResult
    fromIntegral <$> peek valPtr

-- |Parameter management
cuParamSetSize :: CUFunction -> Int -> IO ()
cuParamSetSize (CUFunction funcPtr) size =
    c_cuParamSetSize funcPtr (fromIntegral size) >>= checkCUResult

cuParamSeti :: CUFunction -> Int -> Int -> IO ()
cuParamSeti (CUFunction funcPtr) offset i =
    c_cuParamSeti funcPtr (fromIntegral offset) (fromIntegral i) >>= checkCUResult

cuParamSetf :: CUFunction -> Int -> Float -> IO ()
cuParamSetf (CUFunction funcPtr) offset f =
    c_cuParamSetf funcPtr (fromIntegral offset) (realToFrac f) >>= checkCUResult

cuParamSetv :: CUFunction -> Int -> Ptr a -> Int -> IO ()
cuParamSetv (CUFunction funcPtr) offset ptr size =
    c_cuParamSetv funcPtr (fromIntegral offset) ptr (fromIntegral size) >>= checkCUResult

-- |Launch function
cuLaunch :: CUFunction -> IO ()
cuLaunch (CUFunction funcPtr) =
    c_cuLaunch funcPtr >>= checkCUResult

cuLaunchGrid :: CUFunction -> Int -> Int -> IO ()
cuLaunchGrid (CUFunction funcPtr) w h =
    c_cuLaunchGrid funcPtr (fromIntegral w) (fromIntegral h) >>= checkCUResult

cuLaunchGridAsync :: CUFunction -> Int -> Int -> CUStream -> IO ()
cuLaunchGridAsync (CUFunction funcPtr) w h (CUStream streamPtr) =
    c_cuLaunchGridAsync funcPtr (fromIntegral w) (fromIntegral h) streamPtr >>=
    checkCUResult

-- |Events
cuEventCreate :: IO CUEvent
cuEventCreate =  do
    alloca $ \eventPtrPtr -> do
    c_cuEventCreate eventPtrPtr 0 >>= checkCUResult
    eventPtr <- peek eventPtrPtr
    return (CUEvent eventPtr)

cuEventRecord :: CUEvent -> CUStream -> IO ()
cuEventRecord (CUEvent eventPtr) (CUStream streamPtr) =
    c_cuEventRecord eventPtr streamPtr >>= checkCUResult

cuEventQuery :: CUEvent -> IO CUResult
cuEventQuery (CUEvent eventPtr) =
    (toEnum . fromIntegral) <$> c_cuEventQuery eventPtr

cuEventSynchronize :: CUEvent -> IO ()
cuEventSynchronize (CUEvent eventPtr) =
    c_cuEventSynchronize eventPtr >>= checkCUResult

cuEventDestroy :: CUEvent -> IO ()
cuEventDestroy (CUEvent eventPtr) =
    c_cuEventDestroy eventPtr >>= checkCUResult

cuEventElapsedTime :: CUEvent -> CUEvent -> IO Float
cuEventElapsedTime (CUEvent startEventPtr) (CUEvent endEventPtr) = do
    alloca $ \tPtr -> do
    c_cuEventElapsedTime tPtr startEventPtr endEventPtr >>= checkCUResult
    realToFrac <$> peek tPtr

-- |Streams
defaultStream :: CUStream
defaultStream = CUStream nullPtr

cuStreamCreate :: IO CUStream
cuStreamCreate =  do
    alloca $ \streamPtrPtr -> do
    c_cuStreamCreate streamPtrPtr 0 >>= checkCUResult
    streamPtr <- peek streamPtrPtr
    return (CUStream streamPtr)

cuStreamQuery :: CUStream -> IO CUResult
cuStreamQuery (CUStream streamPtr) =
    (toEnum . fromIntegral) <$> c_cuStreamQuery streamPtr

cuStreamSynchronize :: CUStream -> IO ()
cuStreamSynchronize (CUStream streamPtr) =
    c_cuStreamSynchronize streamPtr >>= checkCUResult

cuStreamDestroy :: CUStream -> IO ()
cuStreamDestroy (CUStream streamPtr) =
    c_cuStreamDestroy streamPtr >>= checkCUResult

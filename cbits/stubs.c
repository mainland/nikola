/* CUDA doesn't properly define CUDARTAPI when using MinGW gcc */
#if defined(mingw32_TARGET_OS)
#include <host_defines.h>
#undef CUDARTAPI
#define CUDARTAPI __stdcall
#endif

#include <cuda.h>

/* We define our own cuMemFree wrapper to avoid issues with stdcall on Win32. */
void cuMemFree_(CUdeviceptr dptr)
{
    cuMemFree(dptr);
}

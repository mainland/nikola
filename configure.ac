AC_INIT([The Nikola embedded language], [0.2], [mainland@eecs.harvard.edu], [nikola])

#
# Portions of this configuration file are taken from the configure.ac included
# as part of the Accelerate library
#

AC_CONFIG_SRCDIR([src/Data/Array/Nikola/Array.hs])
AC_CONFIG_FILES([include/Nikola.h nikola.buildinfo])

AC_CANONICAL_TARGET

#
# Find nvcc and determine its version
#

AC_ARG_WITH([nvcc],
    [AC_HELP_STRING([--with-nvcc], [path to nvcc])],
    [NVCC=$withval],
    [AC_PATH_PROG(NVCC, nvcc)])

if test -z "$NVCC"; then
  AC_MSG_ERROR(could not find nvcc)
fi

NVCC_VERSION=`nvcc --version | grep release | sed -e 's/.*release //' -e 's/,.*//' -e 's/\.//'`

#
# Find the C compiler to use with nvcc
#

AC_ARG_WITH([nvcc-cc],
    [AC_HELP_STRING([--with-nvcc-cc], [path to compiler to use with nvcc])],
    [NVCC_CC=$withval],
    [AC_PATH_PROG(NVCC_CC, cc)])

if test -z "$NVCC_CC"; then
  AC_MSG_ERROR(could not find C compiler to use with nvcc)
fi

#
# Find the C compiler
#

AC_ARG_WITH([gcc],
    [AC_HELP_STRING([--with-gcc], [path to gcc])],
    [CC=$withval],
    [AC_PATH_PROG(CC, gcc)])

#
# Find the Haskell compiler
#

AC_ARG_WITH([compiler],
    [AC_HELP_STRING([--with-compiler], [path to Haskell compiler])],
    [HC=$withval],
    [AC_PATH_PROG(HC, ghc)])

#
# Find CUDA headers and libraries
#

CUDA_PREFIX="$(dirname $(dirname $NVCC))"
CPPFLAGS+=" -I${CUDA_PREFIX}/include "

case $target in
    x86_64*) LDFLAGS+=" -L${CUDA_PREFIX}/lib64 " ;;
    *)       LDFLAGS+=" -L${CUDA_PREFIX}/lib "   ;;
esac

# If we are running on Mac OS add the CUDA library path to the search list. This
# option allows applications to run without requiring to set [DY]LD_LIBRARY_PATH
case $build in
    *darwin* ) LDFLAGS+=" -Xlinker -rpath ${cuda_prefix}/lib " ;;
    * ) ;;
esac

#
# Find the CUDA headers
#
AC_CHECK_HEADERS([cuda.h cuda_runtime_api.h],
    [],
    [AC_MSG_ERROR(could not find CUDA headers)])

AC_CHECK_HEADERS([cuda_gl_interop.h],
    [],
    [AC_MSG_ERROR(could not find CUDA GL interoperability headers)])

AC_CHECK_HEADERS([cudaGL.h],
    [],
    [AC_MSG_ERROR(could not find CUDA GL interoperability headers)],
[
#include <cuda.h>
#include <GL/gl.h>
])

#
# Find the CUDA libraries
#

AC_SEARCH_LIBS(cuDriverGetVersion, cuda, [],
               [AC_MSG_ERROR(could not find CUDA driver library)])

AC_SEARCH_LIBS(cudaRuntimeGetVersion, cudart, [],
               [AC_MSG_ERROR(could not find CUDA runtime library)])

CUDA_CPPFLAGS="$CPPFLAGS"
CUDA_LDFLAGS="$LDFLAGS $LIBS"

#
# Finish up
#

AC_SUBST([CFLAGS])
AC_SUBST([LDFLAGS])
AC_SUBST([LIBS])

AC_SUBST([NVCC_VERSION])

AC_OUTPUT

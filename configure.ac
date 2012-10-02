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
CUDA_C2HSFLAGS=" --cpp=$NVCC --cppopts=-E "
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

AC_CHECK_HEADERS([cuda.h cuda_runtime_api.h],
[],
[AC_MSG_ERROR(could not find CUDA headers)]
)

AC_CHECK_HEADERS([cuda_gl_interop.h],
[],
[AC_MSG_ERROR(could not find CUDA GL interoperability headers)],
[]
)

AC_CHECK_HEADERS([cudaGL.h],
[],
[AC_MSG_ERROR(could not find CUDA GL interoperability headers)],
[#include <cuda.h>
#include <GL/gl.h>
]
)

AC_SEARCH_LIBS(cuDriverGetVersion, cuda, [],
               [AC_MSG_ERROR(could not find CUDA driver library)])

AC_SEARCH_LIBS(cudaRuntimeGetVersion, cudart, [],
               [AC_MSG_ERROR(could not find CUDA runtime library)])

CUDA_CPPFLAGS="$CPPFLAGS"
CUDA_LDFLAGS="$LDFLAGS $LIBS"

#
# Recent versions of Mac OS X (10.6 and later) provides a C extension for
# creating lambda-like closure expressions (blocks), the syntax for which
# confuses the c2hs preprocessor. We disable this by undefining the __BLOCKS__
# macro.
#
AC_MSG_CHECKING(for Apple Blocks extension)
if test -r "/usr/include/stdlib.h"; then
    BLOCKS=`grep __BLOCKS__ /usr/include/stdlib.h`
fi
if test -z "$BLOCKS" ; then
    AC_MSG_RESULT(no)
else
    CUDA_C2HSFLAGS+=" --cppopts=-U__BLOCKS__ "
    AC_MSG_RESULT(yes)
fi

#
# Finish up
#

AC_SUBST([NVCC_VERSION])
AC_SUBST([CUDA_CPPFLAGS])
AC_SUBST([CUDA_LDFLAGS])
AC_SUBST([CUDA_C2HSFLAGS])
AC_OUTPUT

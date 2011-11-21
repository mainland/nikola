AC_INIT([The Nikola embedded language], [0.2], [mainland@eecs.harvard.edu], [nikola])

AC_CONFIG_SRCDIR([src/Nikola.hs])
AC_CONFIG_FILES([nikola.buildinfo include/Nikola.h])

dnl--------------------------------------------------------------------
dnl * Find NVCC
dnl--------------------------------------------------------------------

AC_PATH_PROG(NVCC, nvcc)

if test -z "$NVCC"; then
  AC_MSG_ERROR(could not find nvcc)
fi

dnl--------------------------------------------------------------------
dnl * Deal with arguments telling us cuda is somewhere odd
dnl--------------------------------------------------------------------

AC_ARG_WITH([cuda-includes],
  [AC_HELP_STRING([--with-cuda-includes],
    [directory containing cuda.h])],
    [CUDA_INCLUDE_DIRS=$withval; CPPFLAGS="-I$withval"],
    [CUDA_INCLUDE_DIRS=])

AC_ARG_WITH([cuda-libraries],
  [AC_HELP_STRING([--with-cuda-libraries],
    [directory containing cuda library])],
    [CUDA_LIB_DIRS=$withval; LDFLAGS="-L$withval"],
    [CUDA_LIB_DIRS=])

dnl--------------------------------------------------------------------
dnl * Find CUDA
dnl--------------------------------------------------------------------
AC_CHECK_HEADERS([cuda.h], [], [AC_MSG_ERROR(could not find CUDA headers)])

AC_CHECK_LIB([cuda],  [cuDriverGetVersion],
             [CUDA_LIBS=cuda],
             [AC_MSG_ERROR(could not find CUDA driver library)])

AC_SUBST(CUDA_INCLUDE_DIRS)
AC_SUBST(CUDA_LIB_DIRS)
AC_SUBST(CUDA_LIBS)

AC_OUTPUT

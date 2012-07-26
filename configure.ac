AC_INIT([The Nikola embedded language], [0.2], [mainland@eecs.harvard.edu], [nikola])

AC_CONFIG_SRCDIR([src/Data/Array/Nikola/Array.hs])
AC_CONFIG_FILES([include/Nikola.h])

dnl--------------------------------------------------------------------
dnl * Find nvcc and determine its version
dnl--------------------------------------------------------------------

AC_ARG_WITH([nvcc],
  [AC_HELP_STRING([--with-nvcc],
    [path to nvcc])],
    [NVCC=$withval],
    [])
AC_PATH_PROG(NVCC, nvcc)

if test -z "$NVCC"; then
  AC_MSG_ERROR(could not find nvcc)
fi

NVCC_VERSION=`nvcc --version | grep release | sed -e 's/.*release //' -e 's/,.*//' -e 's/\.//'`
AC_SUBST(NVCC_VERSION)

dnl--------------------------------------------------------------------
dnl * Find the C compiler to use with nvcc
dnl--------------------------------------------------------------------

AC_ARG_WITH([nvcc-cc],
  [AC_HELP_STRING([--with-nvcc-cc],
    [path to compiler to use with nvcc])],
    [NVCC_CC=$withval],
    [])
AC_SUBST(NVCC_CC)

dnl--------------------------------------------------------------------
dnl * Find the C compiler
dnl--------------------------------------------------------------------

AC_ARG_WITH([gcc],
  [AC_HELP_STRING([--with-gcc],
    [path to gcc])],
    [CC=$withval],
    [])
AC_PATH_PROG(CC, gcc)

dnl--------------------------------------------------------------------
dnl * Find the Haskell compiler
dnl--------------------------------------------------------------------

AC_ARG_WITH([compiler],
  [AC_HELP_STRING([--with-compiler],
    [path to Haskell compiler])],
    [HC=$withval],
    [])
AC_PATH_PROG(HC, ghc)

AC_OUTPUT

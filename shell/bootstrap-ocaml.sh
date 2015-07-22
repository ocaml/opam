#!/bin/sh -e

V=ocaml-4.06.0
URL=http://caml.inria.fr/pub/distrib/ocaml-4.06/${V}.tar.gz
if command -v curl > /dev/null; then
  CURL="curl -OLSs"
elif command -v wget > /dev/null; then
  CURL=wget
else
  echo "This script requires curl or wget"
  exit 1
fi
mkdir -p bootstrap
cd bootstrap
if [ ! -e ${V}.tar.gz ]; then
  ${CURL} ${URL}
fi
tar -zxf ${V}.tar.gz
cd ${V}
PATH_PREPEND=
if [ -n "$1" -a -n "${COMSPEC}" -a -x "${COMSPEC}" ] ; then
  LIB_PREPEND=
  INC_PREPEND=

  case "$1" in
    "mingw"|"mingw64")
      BUILD=$1
    ;;
    "msvc")
      BUILD=$1
      if ! command -v ml > /dev/null ; then
        eval `../../shell/msvs-detect --arch=x86`
        if [ -n "${MSVS_NAME}" ] ; then
          PATH_PREPEND="${MSVS_PATH}"
          LIB_PREPEND="${MSVS_LIB};"
          INC_PREPEND="${MSVS_INC};"
        fi
      fi
    ;;
    "msvc64")
      BUILD=$1
      if ! command -v ml64 > /dev/null ; then
        eval `../../shell/msvs-detect --arch=x64`
        if [ -n "${MSVS_NAME}" ] ; then
          PATH_PREPEND="${MSVS_PATH}"
          LIB_PREPEND="${MSVS_LIB};"
          INC_PREPEND="${MSVS_INC};"
        fi
      fi
    ;;
    *)
      if [ "$1" != "auto" ] ; then
        echo "Compiler architecture $1 not recognised -- mingw64, mingw, msvc64, msvc (or auto)"
      fi
      if [ -n "${PROCESSOR_ARCHITEW6432}" -o "${PROCESSOR_ARCHITECTURE}" = "AMD64" ] ; then
        TRY64=1
      else
        TRY64=0
      fi

      if [ ${TRY64} -eq 1 ] && command -v x86_64-w64-mingw32-gcc > /dev/null ; then
        BUILD=mingw64
      elif command -v i686-w64-mingw32-gcc > /dev/null ; then
        BUILD=mingw
      elif [ ${TRY64} -eq 1 ] && command -v ml64 > /dev/null ; then
        BUILD=msvc64
        PATH_PREPEND=`bash ../../shell/check_linker`
      elif command -v ml > /dev/null ; then
        BUILD=msvc
        PATH_PREPEND=`bash ../../shell/check_linker`
      else
        if [ ${TRY64} -eq 1 ] ; then
          BUILD=msvc64
          BUILD_ARCH=x64
        else
          BUILD=msvc
          BUILD_ARCH=x86
        fi
        eval `../../shell/msvs-detect --arch=${BUILD_ARCH}`
        if [ -z "${MSVS_NAME}" ] ; then
          echo "No appropriate C compiler was found -- unable to build OCaml"
          exit 1
        else
          PATH_PREPEND="${MSVS_PATH}"
          LIB_PREPEND="${MSVS_LIB};"
          INC_PREPEND="${MSVS_INC};"
        fi
      fi
    ;;
  esac
  if [ -n "${PATH_PREPEND}" ] ; then
    PATH_PREPEND="${PATH_PREPEND}:"
  fi
  PREFIX=`cd .. ; pwd | cygpath -f - -m`/ocaml
  sed -e "s|^PREFIX=.*|PREFIX=${PREFIX}|" config/Makefile.${BUILD} > config/Makefile
  cp config/s-nt.h byterun/caml/s.h
  cp config/m-nt.h byterun/caml/m.h
  FV=0.37
  cd ..
  if [ ! -e ${FV}.zip ]; then
    ${CURL} https://github.com/alainfrisch/flexdll/archive/${FV}.tar.gz
  fi
  cd ${V}
  tar -xzf ../${FV}.tar.gz
  rm -rf flexdll
  mv flexdll-${FV} flexdll
  PATH="${PATH_PREPEND}${PREFIX}/bin:${PATH}" Lib="${LIB_PREPEND}${Lib}" Include="${INC_PREPEND}${Include}" make flexdll world.opt install
else
  ./configure -prefix "`pwd`/../ocaml"
  ${MAKE:-make} world opt.opt
  ${MAKE:-make} install
fi

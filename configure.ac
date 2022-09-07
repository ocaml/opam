dnl The line below must be formatted AC_INIT(opam,VERSION) with no extra spaces
AC_INIT(opam,2.2.0~alpha~dev)
AC_COPYRIGHT(Copyright 2012-2019 OcamlPro SAS)

AC_CONFIG_MACRO_DIR([m4])

# If a bootstrap compiler has been built, always use it
PATH_PREPEND=
PRE_BOOTSTRAP_PATH="$PATH"
AS_IF([ test -x bootstrap/ocaml/bin/ocamlc -o -x bootstrap/ocaml/bin/ocamlopt ],[
  echo Bootstrap compiler found -- activating
  unset OCAMLLIB
  export PATH_PREPEND=`pwd`/bootstrap/ocaml/bin:
  export PATH="$PATH_PREPEND$PATH"
])

MIN_OCAML_VERSION=4.08.0

AC_PROG_OCAML
AC_CHECK_OCAML_OS_TYPE

AS_IF([test "x${MAKE}" = "x" && test "${OCAML_OS_TYPE}" = "Unix"],[
  MAKE="$(if command -v gmake &> /dev/null; then echo gmake; else echo make; fi)"
],[
  MAKE=${MAKE:-make}
])

if test "x$OCAMLC" = "xno"; then
   AC_MSG_ERROR([You must install the OCaml compiler, at least version $MIN_OCAML_VERSION (or run: $MAKE compiler)])
fi
AS_IF([ test "${OCAML_OS_TYPE}" = "Win32"],[
  OCAMLLIB="$(echo "${OCAMLLIB}" | tr -d '\015' | sed -e 's|\\|/|g')"
  WIN32=1
  EXE=.exe
],[
  EXE=
  WIN32=0
])

AC_SUBST(WIN32)
AC_SUBST(EXE)

# AC_PREFIX_DEFAULT must be at the top level, which is the reason for the
# somewhat convoluted pair of sed expressions...
AC_PREFIX_DEFAULT([`echo "os_type: ${OCAML_OS_TYPE}" | sed -e "s;^os_type: Win32;C:/OPAM;" -e "s;^os_type:.*;/usr/local;"`])

AC_ARG_ENABLE([version_check],
  AS_HELP_STRING([--disable-version-check],
                 [Do not check OCaml version])
)

AC_ARG_ENABLE([checks],
  AS_HELP_STRING([--disable-checks],
                 [Assume OCaml dependencies are OK without checking])
)

AC_ARG_ENABLE([developer_mode],
  AC_HELP_STRING([--enable-developer-mode],
                 [Enable developer features])
)

AC_ARG_WITH([mccs],
  AC_HELP_STRING([--without-mccs],
                 [Compile without a built-in Cudf solver (only works if 'mccs' is not otherwise installed)]),[],[MCCS_DEFAULT=yes]
)
AC_ARG_WITH([0install-solver],
  AC_HELP_STRING([--without-0install-solver],
                 [Legacy option (does nothing)])
)
AS_IF([test "x$with_0install_solver" = xno],
  [AC_MSG_ERROR([Compiling opam without 0install solver is no longer possible.])]
)


AC_ARG_WITH([libacl],
  AC_HELP_STRING([--with-libacl],
                 [Compile opam with libacl support]),,[with_libacl=auto])

AC_ARG_WITH([private_runtime],
  AC_HELP_STRING([--with-private-runtime],
                 [For a mingw-w64 build, manifest the runtime DLLs locally in Opam.Runtime.arch]),,[with_private_runtime=no]
)

AC_ARG_ENABLE([cold_check],
  AC_HELP_STRING([--enable-cold-check],
                 [Fail on some check necessary for make cold]),[COLD_CHECK=yes],[]
)

AC_ARG_WITH([dune],
  AC_HELP_STRING([--without-dune],
                 [Compile using vendored Dune instead of any in PATH]))

AC_ARG_WITH([vendored-deps],
  AC_HELP_STRING([--with-vendored-deps],
                 [Use vendored dependencies instead of any available in the compiler installation]))

AS_IF([test "x" != "x$LIB_PREFIX"], [
  CPATH=$CPATH:$LIB_PREFIX/include
  LIBRARY_PATH=$LIBRARY_PATH:$LIB_PREFIX/lib
  export CPATH LIBRARY_PATH
])
AC_SUBST(LIB_PREFIX,$LIB_PREFIX)
AC_SUBST(CPATH,$CPATH)
AC_SUBST(LIBRARY_PATH,$LIBRARY_PATH)

AS_IF([test "x${enable_version_check}" != "xno"], [
  AX_COMPARE_VERSION(
    [$OCAMLVERSION], [lt], [$MIN_OCAML_VERSION],
    AC_MSG_ERROR([Your version of OCaml ($OCAMLVERSION) is not supported]))
])

AC_MSG_CHECKING([for compiler type])
CCOMP_TYPE=`$OCAML shell/print_config.ml ccomp_type 2>/dev/null | fgrep -v "Cannot find" || $OCAMLC -config | tr -d '\r' | sed -n -e "s/ccomp_type: //p"`
AS_IF([ test "$?" -eq 0 ],,[AC_MSG_ERROR([failed])] )
AC_MSG_RESULT([$CCOMP_TYPE])
AC_MSG_CHECKING([for compiler architecture])
ARCH=`$OCAML shell/print_config.ml arch 2>/dev/null | fgrep -v "Cannot find" || $OCAMLC -config | tr -d '\r' | sed -n -e "s/architecture: //p"`
AS_IF([ test "$?" -eq 0 ],,[AC_MSG_ERROR([failed])] )
AC_MSG_RESULT([$ARCH])
AC_MSG_CHECKING([for compiler system])
SYSTEM=`$OCAML shell/print_config.ml system 2>/dev/null | fgrep -v "Cannot find" || $OCAMLC -config | tr -d '\r' | sed -n -e "s/system: //p"`
AC_MSG_RESULT([$SYSTEM])
AC_SUBST(SYSTEM)

LIB_PREPEND=
INC_PREPEND=
# Need the C compiler used for OCaml (important on Windows, as both x86 and x64 are used)
AS_IF([test "$OCAMLBEST" = "opt"],[OCAMLBESTCC=$OCAMLOPT],[OCAMLBESTCC=$OCAMLC])
OCAML_CC="$($OCAMLBESTCC -config | sed -n -e "s/native_c_compiler: \(.*\) .*/\1/p")"
set dummy ${OCAML_CC}; OCAML_TEST_CC=$2
AS_IF([test ! -x ${OCAML_TEST_CC}],[
  AS_IF([test "x${CCOMP_TYPE}" = "xmsvc"],[
    AS_IF([test "${ARCH}" = "i386"],[SDK_ARCH=x86],[SDK_ARCH=x64])
    AC_MSG_CHECKING([for an installed Microsoft C Compiler for ${SDK_ARCH}])
    eval `PATH="$PRE_BOOTSTRAP_PATH" bash ./shell/msvs-detect --arch=$SDK_ARCH; echo RESULT=$?`
    AS_IF([ test "x$MSVS_NAME" = "x" ], [
      AS_IF([ test ${RESULT} -eq 0 ], [
        AC_MSG_RESULT([from PATH])
      ],[
        AC_MSG_ERROR([no])
      ])
    ],[
      AC_MSG_RESULT([from $MSVS_NAME])
      export PATH_PREPEND="${MSVS_PATH}${PATH_PREPEND}"
      # Note that we put ${MSVS_PATH} here NOT ${PATH_PREPEND} so that the bootstrap path isn't repeated
      export PATH="${MSVS_PATH}$PATH"
      LIB_PREPEND="${MSVS_LIB}"
      INC_PREPEND="${MSVS_INC}"
      export Lib="${MSVS_LIB}$LIB"
      export Include="${MSVS_INC}$INCLUDE"
    ])
  ])
])
AC_PROG_CC(["${OCAML_CC}" gcc cc])

AS_IF([test "x${enable_developer_mode}" = "xyes"],[
  AC_SUBST(DEVELOPER,true)
  echo "-D" > src/core/developer
  echo "DEVELOPER" >> src/core/developer
],[
  AC_SUBST(DEVELOPER,false)
  rm -f src/core/developer
])

AS_IF([test "x${with_mccs}" = "xno"], [AC_SUBST(MCCS_ENABLED,false)], [AC_SUBST(MCCS_ENABLED,true)])

AS_IF([test "x${with_libacl}" != "xno"],[
  have_libacl=yes
  AC_CHECK_HEADER([acl/libacl.h],,[have_libacl=no])
  AC_SEARCH_LIBS([acl_get_perm],[acl],,[have_libacl=no])
  AS_IF([test "x${SYSTEM}" = "xcygwin"],[
    AS_IF([test "x${have_libacl}" = "xno"],[AC_MSG_ERROR([opam on Cygwin requires libacl])],[with_libacl=yes])
  ])
  AS_IF([test "x${with_libacl}${have_libacl}" = "xyesno"],[AC_MSG_ERROR([libacl not found])])
])
AS_IF([test "x${with_libacl}" = "xyes"],[
  AS_IF([test "x${ac_cv_search_acl_get_perm}" = "xnone required"],[
    AC_SUBST(CONF_LIBACL_LINK,[])
  ],[
    AC_SUBST(CONF_LIBACL_LINK,[${ac_cv_search_acl_get_perm}])
  ])
])

CONF_OCAMLFLAGS=''

AS_IF([test "x${CI}" != "x" -o "x${enable_developer_mode}" = "xyes"],[
  AS_IF([test "x${CCOMP_TYPE}" = "xmsvc"],[
    CONF_CFLAGS="\"/WX\""
  ],[
    CONF_CFLAGS="-Werror"
  ])
],[
  CONF_CFLAGS=
])
AC_SUBST(CONF_OCAMLFLAGS)
AC_SUBST(CONF_CFLAGS)

AS_IF([ test ${WIN32} -eq 1 -a "$GCC" = "yes"],[
  # Pre-Visual Studio 2013 Microsoft C compiler adhere to the C89 standard, well
  # at least the bit of it which requires variable declarations to appear before
  # statements. Adding this warning to GCC prevents accidentally using C99 and
  # then getting unexpected C2143 errors from older Microsoft C compilers. I'm
  # not aware of an equivalent option for the Microsoft C compiler.
  CC="$CC -Wdeclaration-after-statement"
  AC_SUBST(CC)
])

AS_IF([ test ${WIN32} -eq 1 ],[
  AS_IF([ test "$GCC" = "yes"], [
    AS_IF([ test "x${CC64}" = "x" ], [
      AS_IF([ test "$ARCH" = "i386" ],[T_CC64=x86_64-w64-mingw32-gcc],[T_CC64=i686-w64-mingw32-gcc])])
    AC_CHECK_TOOL(CC64,[${T_CC64}],[no])
    AS_IF([ test "x${CC64}" != "xno" ],[
      echo "${CC64} -o " > src/stubs/win32/cc64
      echo " -Wdeclaration-after-statement -municode " >> src/stubs/win32/cc64
    ])
  ],[
    AC_MSG_CHECKING([whether Microsoft Linker needs a PATH shim])
    PREV_PREPEND="$PATH_PREPEND"
    PATH_PREPEND=$(bash ./shell/check_linker)
    AS_IF([test "x${PATH_PREPEND}" = "x${PREV_PREPEND}" ],[PATH_PREPEND_RESULT=no],[PATH_PREPEND_RESULT=yes])
    PATH_PREPEND=`echo "${PATH_PREPEND}" | sed -e 's/#/\\\\#/g' -e 's/\\$/$$/g'`
    LIB_PREPEND=`echo ${LIB_PREPEND} | sed -e 's/#/\\\\#/g' -e 's/\\$/$$/g'`
    INC_PREPEND=`echo ${INC_PREPEND} | sed -e 's/#/\\\\#/g' -e 's/\\$/$$/g'`
    AC_SUBST(PATH_PREPEND)
    AC_SUBST(LIB_PREPEND)
    AC_SUBST(INC_PREPEND)
    AC_MSG_RESULT([$PATH_PREPEND_RESULT])
    AS_IF([ test "$ARCH" = "i386" ],[COMP_ARCH=x64],[COMP_ARCH=x86])
    AC_MSG_CHECKING([for a way to invoke an $COMP_ARCH C compiler])
    eval `PATH="$PRE_BOOTSTRAP_PATH" bash ./shell/msvs-detect --arch=$COMP_ARCH`
    AS_IF([ test "x$MSVS_NAME" = "x" ], [
      AC_MSG_RESULT([no])
      CC64=no
    ],[
      AC_MSG_RESULT([from $MSVS_NAME])
      CL_FULL="`PATH="${MSVS_PATH}:${PATH}" which cl | cygpath -f - -w`"
      MSVS_PATH="`echo "${MSVS_PATH}" | cygpath -f - -wp`"
      echo "cl /nologo /Fe" > src/stubs/win32/cc64
      echo " " >> src/stubs/win32/cc64
      echo "PATH" >> src/stubs/win32/cc64
      echo "${MSVS_PATH}" >> src/stubs/win32/cc64
      echo "LIB" >> src/stubs/win32/cc64
      echo "${MSVS_LIB}" >> src/stubs/win32/cc64
      echo "INCLUDE" >> src/stubs/win32/cc64
      echo "${MSVS_INC}" >> src/stubs/win32/cc64
    ])
  ])
  AS_IF([ test "x${CC64}" = "xno" ],[
    AC_MSG_WARN([Complementary C compiler not found - opam-putenv will not be built])
  ])
])

AC_PROG_FINDLIB

AC_ARG_ENABLE([certificate_check],
  AS_HELP_STRING([--disable-certificate-check],
                 [Do not check the certificate of opam's dependency archives])dnl '
)

AS_IF([test "x${enable_certificate_check}" = "xno"], [
  curl_certificate_check=--insecure
  wget_certificate_check=--no-check-certificate
])

AC_CHECK_PROGS(FETCH,[curl wget],no)

AC_CHECK_TOOL(DUNE,dune)
AC_CHECK_TOOL(CPPO,cppo)
AC_CHECK_TOOL(PATCH,patch)
AC_CHECK_TOOL(BUNZIP2,bunzip2)

AX_COMPARE_VERSION([$OCAMLVERSION], [lt], [4.08.0],
  [DUNE_SECONDARY=src_ext/secondary/ocaml/bin/ocaml],
  [DUNE_SECONDARY=])
AC_SUBST([DUNE_SECONDARY])

AS_CASE([$DUNE,$with_dune],
  [,yes],     [AC_MSG_ERROR([Dune was not found in PATH])],
  [*,no],     [DUNE=''],
  [*,|*,yes], [],
  [DUNE="$with_dune"])

if test "x${COLD_CHECK}" = "xyes" ; then
  if test "x$PATCH"  = "x" ; then
    AC_MSG_ERROR([You must have patch installed.])
  fi
  if test "x$BUNZIP2"  = "x" ; then
    AC_MSG_ERROR([You must have bunzip2 installed.])
  fi
fi

AS_IF([test "${OCAML_OS_TYPE}" = "Win32"],[
  AC_MSG_CHECKING([for a workable solution for ln -s])
  ln -s configure conftestLink
  AS_IF([test "`cmd /c dir conftestLink 2>/dev/null | fgrep SYMLINK`" = ""],[LN_S="cp -a"],[LN_S="ln -s"])
  AC_MSG_RESULT([$LN_S])
],[
  LN_S="ln -s"
])
AC_SUBST(LN_S)

if test x"$FETCH" = x"curl" ; then
  AC_SUBST(fetch, "curl $curl_certificate_check -LSs -o \$(2) \$(1)")
elif test x"$FETCH" = x"wget" ; then
  AC_SUBST(fetch, "wget $wget_certificate_check -O \$(2) \$(1)")
elif test x"${enable_checks}" != x"no" ; then
  if ! ${MAKE} -q -C src_ext has-archives 2>/dev/null ; then
    AC_MSG_ERROR([You must have either curl or wget installed.])
  fi
fi

AS_IF([test "x${with_private_runtime}" != "xno"],[
  AS_IF([test ${WIN32} -eq 1 -a "x${CCOMP_TYPE}" = "xcc"],[
    CONF_MANIFEST_O=opam-manifest.o
    AS_IF([ test "$ARCH" = "i386" ],[
      MANIFEST_ARCH=x86
      RUNTIME_GCC_S=libgcc_s_sjlj-1
    ],[
      MANIFEST_ARCH=amd64
      RUNTIME_GCC_S=libgcc_s_seh-1
    ])
    AC_SUBST(MANIFEST_ARCH)
    AC_SUBST(RUNTIME_GCC_S)
  ],[
    CONF_MANIFEST_O=
    AC_MSG_WARN([--with-private-runtime ignored (not building on mingw)])
  ])
],[CONF_MANIFEST_O=])
AC_SUBST(CONF_MANIFEST_O)

echo

# Dependencies
AC_CHECK_OCAML_PKG([unix])
AC_CHECK_OCAML_PKG([bigarray])
AC_CHECK_OCAML_PKG_AT_LEAST([re], [1.9.0])
AC_CHECK_OCAML_PKG_AT_LEAST([base64], [3.1.0])
AC_CHECK_OCAML_PKG([cmdliner])
AC_CHECK_OCAML_PKG([ocamlgraph])
AC_CHECK_OCAML_PKG_AT_LEAST([cudf], [0.7])
AC_CHECK_OCAML_PKG_AT_LEAST([dose3.common], [6.1])
AC_CHECK_OCAML_PKG_AT_LEAST([dose3.algo], [6.1])
AC_CHECK_OCAML_PKG_AT_LEAST([opam-file-format], [2.1.4])
AC_CHECK_OCAML_PKG([spdx_licenses])
AC_CHECK_OCAML_PKG_AT_LEAST([opam-0install-cudf],[0.4])
AC_CHECK_OCAML_PKG([jsonm])
AC_CHECK_OCAML_PKG([uutf])
AC_CHECK_OCAML_PKG([sha])

# Optional dependencies
AC_CHECK_OCAML_PKG_AT_LEAST([mccs],[1.1+9])

AS_IF([test "x${with_mccs}" = "xno" && test "x$OCAML_PKG_mccs" != "xno"],
      [AC_MSG_ERROR([Option --without-mccs is not available without uninstalling the 'mccs' package])])

dnl -- that's what we would like to do, but no way to disable mccs in jbuilder
dnl -- if it's installed, at the moment
dnl AS_IF([test "x${with_mccs}" != "xno"],
dnl      AC_CHECK_OCAML_PKG([mccs]),
dnl      [echo "checking for OCaml findlib package mccs... disabled"
dnl       AC_SUBST(OCAML_PKG_mccs,"no")])

AS_IF([test "x$MCCS_ENABLED" = "xtrue"],[
  AS_IF([test "x${CCOMP_TYPE}" != "xmsvc"],[
    AC_PROG_CXX
  ])
])

# Curiously, autoconf doesn't actually detect a C++ compiler!
AS_IF([test -n "$CXX"],[
  CHECK_CXX_WORKS
  AS_IF([test -z "$CXX"],[
    AS_IF([test "x$MCCS_DEFAULT" = "xyes"],[
      AC_SUBST(MCCS_ENABLED,false)
    ],[
      AS_IF([test "x${enable_checks}" != "xno"],[
        AC_MSG_ERROR([A C++ compiler is required to build mccs])
      ])
    ])
  ])
])

echo

AS_IF([test "x${enable_checks}" != "xno" && {
       test "x$OCAML_PKG_extlib" = "xno" ||
       test "x$OCAML_PKG_re" = "xno" ||
       test "x$OCAML_PKG_base64" = "xno" ||
       test "x$OCAML_PKG_cmdliner" = "xno" ||
       test "x$OCAML_PKG_ocamlgraph" = "xno" ||
       test "x$OCAML_PKG_cudf" = "xno" ||
       test "x$OCAML_PKG_dose3_common" = "xno" ||
       test "x$OCAML_PKG_dose3_algo" = "xno" ||
       test "x$OCAML_PKG_opam_file_format" = "xno" ||
       test "x$OCAML_PKG_spdx_licenses" = "xno" ||
       test "x$OCAML_PKG_opam_0install_cudf" = "xno" ||
       test "x$OCAML_PKG_jsonm" = "xno" ||
       test "x$OCAML_PKG_uutf" = "xno" ||
       test "x$OCAML_PKG_sha" = "xno" ||
       test "x$CPPO" = "x" ||
       test "x$OCAML_PKG_mccs$MCCS_ENABLED" = "xnotrue";}],[
  AS_IF([test "x${with_vendored_deps}" = "xno"],[
    AC_MSG_ERROR([Dependencies missing and --without-vendored-deps given; override with --disable-checks])
  ],[
    AS_IF([test "x${with_vendored_deps}" != "xyes"],[
      echo "============================================================================"
      echo "Some dependencies are missing. Vendored mode has been automatically enabled."
      echo "The sources for opam's dependencies will be extracted in src_ext and built."
      echo "============================================================================"
    ])
    AC_SUBST(VENDORED,"true")
  ])
],[
  AS_IF([test "x${with_vendored_deps}" = "xyes"],[
    AC_SUBST(VENDORED,"true")
  ],[
    AC_SUBST(VENDORED,"false")
  ])
])

if test "x$prefix" = "xNONE"; then
  prefix=$ac_default_prefix
fi

AC_CONFIG_FILES(
  Makefile.config
  src/ocaml-flags-configure.sexp
  src/stubs/c-flags.sexp
  src/stubs/libacl/c-libraries.sexp
)
AC_OUTPUT

rm -f src/manifest/dune src/manifest/install.inc
AS_IF([test "x${with_private_runtime}" != "xno"],[
  AS_IF([test ${WIN32} -eq 1 -a "x${CCOMP_TYPE}" = "xcc"],[
    cd src/manifest
    ${LN_S} -f dune-manifest dune
    ${LN_S} -f install.${ARCH} install.inc
    cd ../..
  ],[
    AC_MSG_WARN([--with-private-runtime ignored (not building on mingw)])
  ])
])

rm -f src/stubs/win32/dune
AS_IF([ test ${WIN32} -eq 1],[
  cd src/stubs/win32
  ${LN_S} -f dune-win32 dune
  cd ../../..
])
rm -f src/stubs/libacl/dune
AS_IF([ test "x${with_libacl}" = "xyes" ],[
  cd src/stubs/libacl
  ${LN_S} -f dune-libacl dune
  cd ../../..
])

echo

bindir="`eval echo ${bindir}`"
bindir="`eval echo ${bindir}`"
mandir="`eval echo ${mandir}`"
mandir="`eval echo ${mandir}`"

AS_IF([test "x$MCCS_ENABLED" = "xfalse"],[
  echo "Opam will be built WITHOUT its default built-in solver"
],[
  echo "Opam will be built WITH its default built-in solver"
])
echo
echo Executables will be installed in ${bindir}
echo Manual pages will be installed in ${mandir}

AS_IF([test "x${VENDORED}" = "xtrue"],[
  echo
  AS_ECHO_N(["Downloading vendored source dependencies... "])
  ${MAKE} --no-print-directory -C src_ext SILENT=@ archives
  AS_ECHO([done])
  AS_ECHO_N(["Extracting vendored source dependencies in src_ext/... "])
  ${MAKE} --no-print-directory -C src_ext SILENT=@ clone
  AS_ECHO([done])
],[
  ${MAKE} --no-print-directory -C src_ext def-ignore
  AS_IF([test -z "${DUNE}"],[
    echo
    AS_ECHO_N(["Downloading dune sources... "])
    ${MAKE} --no-print-directory -C src_ext SILENT=@ dune-local.download
    AS_ECHO([done])
    AS_ECHO_N(["Extracting dune sources to src_ext/dune-local/... "])
    ${MAKE} --no-print-directory -C src_ext SILENT=@ dune-local.stamp
    AS_ECHO([done])
  ])
])

AC_INIT(opam,2.0.0~beta3)
AC_COPYRIGHT(Copyright 2012-2017 OcamlPro SAS)

AC_CONFIG_MACRO_DIR([m4])

AC_PROG_OCAML
if test "x$OCAMLC" = "xno"; then
   AC_MSG_ERROR([You must install the OCaml compiler])
fi
AC_CHECK_OCAML_OS_TYPE
if test "${OCAML_OS_TYPE}" = "Win32" ; then
  OCAMLLIB="$(echo "${OCAMLLIB}" | tr -d '\015' | sed -e 's|\\|/|g')"
fi

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

# Check that OCaml version is greater or equal to 4.02.3
AS_IF([test "x${enable_version_check}" != "xno"], [
  AX_COMPARE_VERSION(
    [$OCAMLVERSION], [lt], [4.02.3],
    AC_MSG_ERROR([Your version of OCaml: $OCAMLVERSION is not supported]))
])

# Need the C compiler used for OCaml (important on Windows, as both x86 and x64 are used)
AS_IF([test "$OCAMLBEST" = "opt"],[OCAMLBESTCC=$OCAMLOPT],[OCAMLBESTCC=$OCAMLC])
AC_PROG_CC([$($OCAMLBESTCC -config | sed -n -e "s/native_c_compiler: \(.*\) .*/\1/p") gcc cl cc])

AS_IF([test "x${enable_developer_mode}" = "xyes"], [AC_SUBST(DEVELOPER,true)], [AC_SUBST(DEVELOPER,false)])

AS_IF([test "x${CI}" != "x" -o "x${enable_developer_mode}" = "xyes"], [
  CONF_OCAMLFLAGS="-warn-error A"
],[
  CONF_OCAMLFLAGS=
])
AC_SUBST(CONF_OCAMLFLAGS)

AX_COMPARE_VERSION([$OCAMLVERSION], [ge], [4.02.0])
AC_SUBST(ocaml_4_02,"$ax_compare_version")
AX_COMPARE_VERSION([$OCAMLVERSION], [ge], [4.03.0])
AC_SUBST(ocaml_4_03,"$ax_compare_version")

AS_IF([ test "${OCAML_OS_TYPE}" = "Win32"],[
  WIN32=1
  EXE=.exe
],[
  EXE=
  WIN32=0
])

AC_SUBST(WIN32)
AC_SUBST(EXE)

AS_IF([ test ${WIN32} -eq 1 -a "$GCC" != "yes" ],[
  AC_MSG_CHECKING([whether Microsoft Linker needs a PATH shim])
  PATH_PREPEND=$(bash ./shell/check_linker)
  AS_IF([test "x${PATH_PREPEND}" = "x" ],[PATH_PREPEND_RESULT=no],[PATH_PREPEND_RESULT=yes])
  AC_SUBST(PATH_PREPEND)
  AC_MSG_RESULT([$PATH_PREPEND_RESULT])
])

AC_PROG_FINDLIB

AC_ARG_ENABLE([certificate_check],
  AS_HELP_STRING([--disable-certificate-check],
                 [Do not check the certificate of opam's dependency archives])
)

AS_IF([test "x${enable_certificate_check}" = "xno"], [
  curl_certificate_check=--insecure
  wget_certificate_check=--no-check-certificate
])

AC_CHECK_PROGS(FETCH,[curl wget],no)

AC_CHECK_TOOL(JBUILDER,jbuilder)

if test x"$FETCH" = x"curl" ; then
  AC_SUBST(fetch, "curl $curl_certificate_check -OL")
elif test x"$FETCH" = x"wget" ; then
  AC_SUBST(fetch, "wget $wget_certificate_check")
elif test x"${enable_checks}" != x"no" ; then
  AC_MSG_ERROR([You must have either curl or wget installed.])
fi

echo

AC_CHECK_OCAML_PKG([unix])
AC_CHECK_OCAML_PKG([bigarray])
AC_CHECK_OCAML_PKG([extlib])
AC_CHECK_OCAML_PKG([re])
AC_CHECK_OCAML_PKG([re.glob])
AC_CHECK_OCAML_PKG([cmdliner])
AC_CHECK_OCAML_PKG([ocamlgraph])
AC_CHECK_OCAML_PKG([cudf])
AC_CHECK_OCAML_PKG(dose3.common,dose.common)
AC_CHECK_OCAML_PKG(dose3.algo,dose.algo)
AC_CHECK_OCAML_PKG([opam-file-format])
AC_CHECK_OCAML_PKG([mccs])

dnl echo
dnl echo "extlib........................ ${OCAML_PKG_extlib}"
dnl echo "re............................ ${OCAML_PKG_re}"
dnl echo "cmdliner...................... ${OCAML_PKG_cmdliner}"
dnl echo "graph......................... ${OCAML_PKG_ocamlgraph}"
dnl echo "cudf.......................... ${OCAML_PKG_cudf}"
dnl echo "dose3......................... ${OCAML_PKG_dose3}"
echo


AS_IF([test "x${enable_checks}" != "xno" && {
       test "x$OCAML_PKG_extlib" = "xno" ||
       test "x$OCAML_PKG_re" = "xno" ||
       test "x$OCAML_PKG_cmdliner" = "xno" ||
       test "x$OCAML_PKG_ocamlgraph" = "xno" ||
       test "x$OCAML_PKG_cudf" = "xno" ||
       test "x$OCAML_PKG_dose3_common" = "xno" ||
       test "x$OCAML_PKG_opam_file_format" = "xno"; }],[
  echo "============================================================================"
  echo "Some dependencies are missing. If you are just interested in the stand-alone"
  echo "'opam' binary, run 'make lib-ext' to download and include them."
  echo "============================================================================"
  echo
  AC_SUBST(hasalldeps,"")
],[
  AC_SUBST(hasalldeps,"true")
])

if test "x$prefix" = "xNONE"; then
  prefix=$ac_default_prefix
fi

AC_CONFIG_FILES(
  Makefile.config
)
AC_OUTPUT

echo

bindir="`eval echo ${bindir}`"
bindir="`eval echo ${bindir}`"
mandir="`eval echo ${mandir}`"
mandir="`eval echo ${mandir}`"

echo Executables will be installed in ${bindir}
echo Manual pages will be installed in ${mandir}
